%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-11-02
%%
%% @doc Send e-mail to a recipient. Optionally queue low priority messages.

-module(z_email).
-author("Marc Worrell <marc@worrell.nl>").

%% interface functions
-export([
	get_admin_email/1,
	send_admin/3,
	
    send/4,
    send_render/4,
    send_render/5,

    sendq/4,
    sendq_render/4,
    sendq_render/5
]).


%% @doc Fetch the e-mail address of the site administrator
get_admin_email(Context) ->
	case m_config:get_value(zotonic, admin_email, Context) of
		undefined -> 
			case m_site:get(admin_email, Context) of
				undefined -> hd(string:tokens("wwwadmin@" ++ z_convert:to_list(m_site:get(hostname, Context)), ":"));
				Email -> Email
			end;
		Email -> Email
	end.

%% @doc Send a simple text message to the administrator
send_admin(Subject, Message, Context) ->
	case get_admin_email(Context) of
		undefined -> error;
		Email -> z_notifier:notify1({email, true, Email, Subject, Message}, Context)
	end.

%% @doc Send a simple text message to an email address
send(To, Subject, Message, Context) ->
	z_notifier:notify1({email, true, To, Subject, Message}, Context).

%% @doc Queue a simple text message to an email address
sendq(To, Subject, Message, Context) ->
	z_notifier:notify1({email, false, To, Subject, Message}, Context).

%% @doc Send a html message to an email address, render the message using a template.
send_render(To, HtmlTemplate, Vars, Context) ->
    send_render(To, HtmlTemplate, undefined, Vars, Context).

%% @doc Send a html and text message to an email address, render the message using two templates.
send_render(To, HtmlTemplate, TextTemplate, Vars, Context) ->
	z_notifier:notify1({email_render, true, To, HtmlTemplate, TextTemplate, Vars}, Context).

%% @doc Queue a html message to an email address, render the message using a template.
sendq_render(To, HtmlTemplate, Vars, Context) ->
    sendq_render(To, HtmlTemplate, undefined, Vars, Context).

%% @doc Queue a html and text message to an email address, render the message using two templates.
sendq_render(To, HtmlTemplate, TextTemplate, Vars, Context) ->
	z_notifier:notify1({email_render, false, To, HtmlTemplate, TextTemplate, Vars}, Context).

