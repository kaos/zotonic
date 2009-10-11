%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Redirect to a preview for media items; for use in the tinyMCE media plugin.

-module(resource_admin_media_preview).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
         init/1,
         resource_exists/2,
         is_authorized/2,
         content_types_provided/2,
         to_image/2
        ]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init([]) -> {ok, []}.


is_authorized(ReqData, _Context) ->
    Context  = z_context:new(ReqData, ?MODULE),
    z_auth:wm_is_authorized(ReqData, Context).


resource_exists(ReqData, Context) ->
    case z_context:get_q("id", Context) of
        undefined ->
            {false, ReqData, Context};
        [] ->
            {false, ReqData, Context};
        Id ->
            case m_rsc:exists(Id, Context) of 
                true ->
                    case m_rsc:is_visible(Id, Context) of
                        true ->
                            Context2 = z_context:set("id", Id, Context),
                            {true, ReqData, Context2};
                        false ->
                            {false, ReqData, Context}
                    end;
                false ->
                    {false, ReqData, Context}
            end
    end.


content_types_provided(ReqData, Context) ->
    {[{"image/jpeg", to_image}], ReqData, Context}.


to_image(ReqData, Context) ->
    Opts = [{width, 100}, {height, 100}],
    Id = z_convert:to_integer(z_context:get("id", Context)),
    {ok, Url} = z_media_tag:url(Id, Opts, Context),
    ReqData1 = wrq:set_resp_header("Location", binary_to_list(Url), ReqData),
    {{halt, 303}, ReqData1, Context}.
