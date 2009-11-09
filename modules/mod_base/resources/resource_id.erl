%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell <marc@worrell.nl>
%% @date 2009-04-28
%%
%% @doc Redirect to resources depening on the content type requested.
%% @todo Split the redirect function to z_context and check for injection in the host header

-module(resource_id).

-author("Tim Benniks <tim@timbenniks.com>").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,
	service_available/2,
    resource_exists/2,
    content_types_provided/2,
    see_other/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    ContextQs = z_context:ensure_qs(Context1),
    Id = z_context:get_q("id", ContextQs),
    ?WM_REPLY(m_rsc:exists(Id, ContextQs), ContextQs).

content_types_provided(ReqData, Context) ->
	{CT,Context1} = get_content_types(Context),
	CT1 = [{Mime, see_other} || {Mime, _Dispatch} <- CT],
	{CT1, ReqData, Context1}.

see_other(ReqData, Context) ->
	Mime = wrq:get_resp_header("Content-Type", ReqData),
    Context1 = ?WM_REQ(ReqData, Context),
	{CT,Context2} = get_content_types(Context1),
    Id = z_context:get_q("id", Context2),
	Location = case proplists:get_value(Mime, CT) of
					page_url -> m_rsc:p_no_acl(Id, page_url, Context2);
					Dispatch -> z_dispatcher:url(Dispatch, [{id,Id}], Context2)
			   end,
	AbsUrl = z_context:abs_url(Location, Context2),
    Context3 = z_context:set_resp_header("Location", AbsUrl, Context2),
	?WM_REPLY({halt, 303}, Context3).

%% @doc Fetch the list of content types provided, together with their dispatch rule name.
get_content_types(Context) ->
	case z_context:get(content_types_dispatch, Context) of
		undefined ->
			CT = z_notifier:foldr({content_types_dispatch}, [], Context),
			?DEBUG(CT),
			CT1 = case proplists:get_value("text/html", CT) of
					undefined -> [{"text/html", page_url}|CT];
					Prov -> [Prov|CT]
				  end,
			Context1 = z_context:set(content_types_dispatch, CT1, Context),
			{CT1, Context1};
		CT -> 
			{CT, Context}
	end.