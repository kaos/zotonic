%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-05-02
%%
%% @doc Email server.  Queues, renders and sends e-mails.

-module(mod_emailer).
-author("Marc Worrell <marc@worrell.nl").
-behaviour(gen_server).

-mod_title("e-Mail sending").
-mod_description("Provided sending of e-mails. e-Mails are queued before sending.").
-mod_prio(1000).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    observe/2,
    send/4,
    send_render/4,
    send_render/5
]).

-include_lib("zophrenic.hrl").
-include_lib("esmtp/include/esmtp_mime.hrl").

-define(SMTP_PORT_TLS, 587).
-define(SMTP_PORT_SSL, 465).

-define(EMAILER_FROM, "mworrell@wanadoo.nl").
-define(EMAILER_HOST, "smtp.wanadoo.nl").
-define(EMAILER_EHLO, "localhost").

% Maximum times we retry to send a message before we mark it as failed.
-define(MAX_RETRY, 7).


-record(state, {from, ehlo, host, ssl, port, username, password, context}).


%% @doc Receive a notification for sending email.
observe({email, To, Subject, Message}, Context) ->
    send(To, Subject, Message, Context);

%% @doc Receive a notification for sending email that is rendered using a template.
observe({email_render, To, HtmlTemplate, Vars}, Context) ->
    send_render(To, HtmlTemplate, Vars, Context);

%% @doc Send a html and text message to an email address, render the message using two templates.
observe({email_render, To, HtmlTemplate, TextTemplate, Vars}, Context) ->
    send_render(To, HtmlTemplate, TextTemplate, Vars, Context).



%% @doc Send a simple text message to an email address
send(To, Subject, Message, Context) ->
    Context1 = zp_context:prune_for_scomp(?ACL_VIS_USER, Context),
    gen_server:cast(?MODULE, {send, To, Subject, Message, Context1}).


%% @doc Send a html message to an email address, render the message using a template.
send_render(To, HtmlTemplate, Vars, Context) ->
    send_render(To, HtmlTemplate, undefined, Vars, Context).

%% @doc Send a html and text message to an email address, render the message using two templates.
send_render(To, HtmlTemplate, TextTemplate, Vars, Context) ->
    Context1 = zp_context:prune_for_scomp(?ACL_VIS_USER, Context),
    gen_server:cast(?MODULE, {send_render, To, HtmlTemplate, TextTemplate, Vars, Context1}).



%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    zp_notifier:observe(email,       {?MODULE, observe}, Context),
    zp_notifier:observe(emai_render, {?MODULE, observe}, Context),
    timer:send_interval(60000, poll),
    {ok, #state{
        from     = m_config:get_value(?MODULE, from, ?EMAILER_FROM, Context),
        host     = m_config:get_value(?MODULE, host, ?EMAILER_HOST, Context),
        port     = zp_convert:to_integer(m_config:get_value(?MODULE, port, 25, Context)),
        ssl      = zp_convert:to_bool(m_config:get_value(?MODULE, ssl, false, Context)),
        ehlo     = m_config:get_value(?MODULE, ehlo, ?EMAILER_EHLO, Context),
        username = m_config:get_value(?MODULE, username, Context),
        password = m_config:get_value(?MODULE, password, Context),
        context  = zp_context:prune_for_database(Context)
    }}.


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
handle_cast({send, To, Subject, Message, Context}, State) ->
    Cols = [
        {recipient, To},
        {render, false},
        {subject, Subject},
        {message, Message},
        {context, zp_context:pickle(Context)},
        {retry, 0}
    ],
    {ok, Id} = zp_db:insert(emailq, Cols, Context),
    send_queued([{id, Id}|Cols], State),
    {noreply, State};

handle_cast({send_render, To, HtmlTemplate, TextTemplate, Vars, Context}, State) ->
    Cols = [
        {recipient, To},
        {render, true},
        {html_tpl, HtmlTemplate},
        {text_tpl, TextTemplate},
        {vars, Vars},
        {context, zp_context:pickle(Context)},
        {retry, 0}
    ],
    {ok, Id} = zp_db:insert(emailq, Cols, Context),
    send_queued([{id, Id}|Cols], State),
    {noreply, State};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Poll the database queue for any retrys.
%% @todo We should have a context per db, to be implemented with the multi-homing :)
handle_info(poll, State) ->
    poll_queued(State#state.context, State),
    {noreply, State};

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    zp_notifier:detach(email,       {?MODULE, observe}, State#state.context),
    zp_notifier:detach(emai_render, {?MODULE, observe}, State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

poll_queued(Context, State) ->
    % Set all messages with too high retry count to 'failed'
    zp_db:q("update emailq set status = 'fail' where status = 'new' and retry > $1", [?MAX_RETRY], Context),
    % Fetch a batch of message to be retried
    Ms = zp_db:assoc_props("select * from emailq where status = 'new' and retry_on < now() order by retry_on asc limit 10", Context),
    [ send_queued(M, State) || M <- Ms ].


send_queued(Cols, State) ->
    {id, Id} = proplists:lookup(id, Cols),
    {recipient, To} = proplists:lookup(recipient, Cols),
    {context, PickledContext} = proplists:lookup(context, Cols),
    Context = zp_context:depickle(PickledContext),
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
                            zp_convert:to_list(To), 
                            State#state.from, 
                            zp_convert:to_list(Subject), 
                            zp_convert:to_list(Msg)),
        sendemail(MimeMsg, State),
        mark_sent(Id, Context)
    end,
    spawn(F).


spawn_send_html(Id, Retry, To, HtmlTemplate, TextTemplate, Vars, Context, State) ->
    F = fun() ->
        mark_retry(Id, Retry, Context),
        
        HtmlOutput = zp_template:render(zp_convert:to_list(HtmlTemplate), Vars, Context),
        Html = binary_to_list(list_to_binary(HtmlOutput)),
        Text = case TextTemplate of
            undefined -> 
                undefined;
            _ ->
                TextOutput = zp_template:render(zp_convert:to_list(TextTemplate), Vars, Context),
                binary_to_list(list_to_binary(TextOutput))
        end,

        % Fetch the subject from the title of the HTML part
        {match, [_, {Start,Len}|_]} = re:run(Html, "<title>(.*)</title>", [dotall, caseless]),
        Subject = string:strip(zp_string:line(lists:sublist(Html, Start+1, Len))),

        % Build the message and send it
        MimeMsg = esmtp_mime:msg(zp_convert:to_list(To), State#state.from, Subject),
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
    zp_db:q("update emailq set status = 'sent', sent = now() where id = $1", [Id], Context).

mark_retry(Id, Retry, Context) ->
    Period = period(Retry),
    zp_db:q("update emailq set retry = retry+1, retry_on = now() + interval '"++integer_to_list(Period)++" minutes' where id = $1", [Id], Context).
    
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
    To1 = string:strip(zp_string:line(binary_to_list(zp_convert:to_binary(To)))),
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

