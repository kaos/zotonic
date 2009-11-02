%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-05-02
%%
%% @doc Email server.  Queues, renders and sends e-mails.

-module(mod_emailer).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("e-Mail sending").
-mod_description("Provided sending of e-mails. e-Mails are queued before sending.").
-mod_prio(1000).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
]).

-include_lib("zotonic.hrl").
-include_lib("esmtp/include/esmtp_mime.hrl").

%% -define(SMTP_PORT_TLS, 587).
%% -define(SMTP_PORT_SSL, 465).

% Maximum times we retry to send a message before we mark it as failed.
-define(MAX_RETRY, 7).

-record(state, {from, ehlo, host, ssl, port, username, password, context}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    z_notifier:observe(email,        self(), Context),
    z_notifier:observe(email_render, self(), Context),
    timer:send_interval(60000, poll),
	State = update_config(#state{context=z_context:new(Context)}),
	{ok, State}.
	

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Send an e-mail to an e-mail address.
handle_cast({{email, SendNow, To, Subject, Message}, Context}, State) ->
	State1 = update_config(State),
    Cols = [
        {recipient, To},
        {render, false},
        {subject, Subject},
        {message, Message},
        {context, z_context:pickle(Context)},
        {retry, 0}
    ],
    {ok, Id} = z_db:insert(emailq, Cols, Context),
	case SendNow of
		true -> send_queued([{id, Id}|Cols], State1);
		false -> ok
	end,
    {noreply, State1};

%% @doc Render a template and send it as an e-mail.
handle_cast({{email_render, SendNow, To, HtmlTemplate, Vars}, Context}, State) ->
	handle_cast({{email_render, SendNow, To, HtmlTemplate, undefined, Vars}, Context}, State);
handle_cast({{email_render, SendNow, To, HtmlTemplate, TextTemplate, Vars}, Context}, State) ->
	State1 = update_config(State),
    Cols = [
        {recipient, To},
        {render, true},
        {html_tpl, HtmlTemplate},
        {text_tpl, TextTemplate},
        {vars, Vars},
        {context, z_context:pickle(Context)},
        {retry, 0}
    ],
    {ok, Id} = z_db:insert(emailq, Cols, Context),
	case SendNow of
		true -> send_queued([{id, Id}|Cols], State1);
		false -> ok
	end,
    {noreply, State1};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @doc Poll the database queue for any retrys.
handle_info(poll, State) ->
    State1 = poll_queued(State),
    {noreply, State1};

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    z_notifier:detach(email,        self(), State#state.context),
    z_notifier:detach(email_render, self(), State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc Refetch the emailer configuration so that we adapt to any config changes.
update_config(State) ->
	%% Make the default no-reply e-mail address for the main site url.
	[EmailFrom|_] = string:tokens("no-reply@" ++ z_convert:to_list(m_site:get(hostname, State#state.context)), ":"),

	%% Let the defaults be overruled by the config settings (from the admin and site config)
    State#state{
        from     = z_convert:to_list(m_config:get_value(?MODULE, email_from, EmailFrom, State#state.context)),
        host     = z_convert:to_list(m_config:get_value(?MODULE, smtp_host, State#state.context)),
        port     = z_convert:to_integer(m_config:get_value(?MODULE, smtp_port, 25, State#state.context)),
        ssl      = z_convert:to_bool(m_config:get_value(?MODULE, smtp_ssl, false, State#state.context)),
        ehlo     = z_convert:to_list(m_config:get_value(?MODULE, smtp_ehlo, "localhost", State#state.context)),
        username = z_convert:to_list(m_config:get_value(?MODULE, smtp_username, State#state.context)),
        password = z_convert:to_list(m_config:get_value(?MODULE, smtp_password, State#state.context))
    }.


%% @doc Fetch a new batch of queued e-mails. Set status of failed messages.
poll_queued(State) ->
    % Set all messages with too high retry count to 'failed'
    z_db:q("update emailq set status = 'fail' where status = 'new' and retry > $1", [?MAX_RETRY], State#state.context),
    % Fetch a batch of messages for sending
    Ms = z_db:assoc_props("select * from emailq where status = 'new' and retry_on < now() order by retry_on asc limit 100", State#state.context),
	case Ms of
		[] -> 
			State;
		_  ->
			State1 = update_config(State), 
    		[ send_queued(M, State) || M <- Ms ],
			State1
	end.


send_queued(Cols, State) ->
    {id, Id} = proplists:lookup(id, Cols),
    {recipient, To} = proplists:lookup(recipient, Cols),
    {context, PickledContext} = proplists:lookup(context, Cols),
    Context = z_context:depickle(PickledContext),
    {retry, Retry} = proplists:lookup(retry, Cols),
    case proplists:get_value(render, Cols) of
        true ->
            {html_tpl, HtmlTemplate} = proplists:lookup(html_tpl, Cols),
            TextTemplate = proplists:get_value(text_tpl, Cols),
            {vars, Vars} = proplists:lookup(vars, Cols),
            spawn_send_html(Id, Retry, To, HtmlTemplate, TextTemplate, Vars, Context, State);
        false ->
            {subject, Subject} = proplists:lookup(subject, Cols),
            {message, Message} = proplists:lookup(message, Cols),
            spawn_send(Id, Retry, To, Subject, Message, Context, State)
    end.


% @doc Spawn a simple mail sending process
spawn_send(Id, Retry, To, Subject, Msg, Context, State) ->
    F = fun() ->
        mark_retry(Id, Retry, Context),
        MimeMsg = esmtp_mime:msg(
                            z_convert:to_list(To), 
                            State#state.from, 
                            z_convert:to_list(Subject), 
                            z_convert:to_list(Msg)),
        sendemail(MimeMsg, State),
        mark_sent(Id, Context)
    end,
    spawn(F).


spawn_send_html(Id, Retry, To, HtmlTemplate, TextTemplate, Vars, Context, State) ->
    F = fun() ->
        mark_retry(Id, Retry, Context),
        
        {HtmlOutput,_HtmlContext} = z_template:render_to_iolist(z_convert:to_list(HtmlTemplate), Vars, Context),
        Html = binary_to_list(iolist_to_binary(HtmlOutput)),
        Text = case TextTemplate of
            undefined -> 
                undefined;
            _ ->
                {TextOutput,_TextContext} = z_template:render_to_iolist(z_convert:to_list(TextTemplate), Vars, Context),
                binary_to_list(iolist_to_binary(TextOutput))
        end,

        % Fetch the subject from the title of the HTML part
        {match, [_, {Start,Len}|_]} = re:run(Html, "<title>(.*)</title>", [dotall, caseless]),
        Subject = string:strip(z_string:line(lists:sublist(Html, Start+1, Len))),

        % Build the message and send it
        MimeMsg = esmtp_mime:msg(z_convert:to_list(To), State#state.from, Subject),
        MimeMsg2 = case Text of
            undefined -> MimeMsg;
            _ -> esmtp_mime:add_text_part(MimeMsg, Text)
        end,
        MimeMsg3 = esmtp_mime:add_html_part(MimeMsg2, Html),
        sendemail(MimeMsg3, State),
        mark_sent(Id, Context)
    end,
    spawn(F).


mark_sent(Id, Context) ->
    z_db:q("update emailq set status = 'sent', sent = now() where id = $1", [Id], Context).

mark_retry(Id, Retry, Context) ->
    Period = period(Retry),
    z_db:q("update emailq set retry = retry+1, retry_on = now() + interval '"++integer_to_list(Period)++" minutes' where id = $1", [Id], Context).
    
    period(0) -> 10;
    period(1) -> 60;
    period(2) -> 12 * 60;
    period(3) -> 24 * 60;
    period(4) -> 48 * 60;
    period(5) -> 72 * 60;
    period(_) -> 7 * 24 * 60.       % Retry every week for extreme cases
    
         
sendemail(Msg = #mime_msg{}, State) ->
    Login = case State#state.username of
        undefined -> no_login;
		[] -> no_login;
        _ -> {State#state.username, State#state.password}
    end,
    MX = {
        State#state.host,
        State#state.port,
        State#state.ssl,
        Login
    },
    Ehlo = State#state.ehlo,
    sendemail(MX, Ehlo, esmtp_mime:from(Msg), esmtp_mime:to(Msg), esmtp_mime:encode(Msg)).
    
sendemail({Host,Port,SSL,Login}, Ehlo, From, To, Msg) ->
    To1 = string:strip(z_string:line(binary_to_list(z_convert:to_binary(To)))),
    {ok, Fsm} = esmtp_fsm:start_link(Host, Port, SSL),
    {ok, _} = esmtp_fsm:ehlo(Fsm, Ehlo),
    case Login of
        {User,Pass} -> {ok, _} = esmtp_fsm:login(Fsm,User,Pass);
        no_login -> ok
    end,
    {ok, _} = esmtp_fsm:mail_from(Fsm, From),
    {ok, _} = esmtp_fsm:rcpt_to(Fsm,To1),
    {ok, _} = esmtp_fsm:message(Fsm,Msg),
    ok = esmtp_fsm:close(Fsm).

