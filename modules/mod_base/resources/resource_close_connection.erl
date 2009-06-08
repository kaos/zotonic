%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-05-13
%%
%% @doc Simple resource that closes the connection, used when uploading forms with Ajax in Safari browsers.

-module(resource_close_connection).
-author("Marc Worrell <marc@worrell.nl").

-export([
    init/1,
    to_html/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("zophrenic.hrl").

init([]) -> {ok, []}.

to_html(ReqData, State) ->
    ReqData2 = wrq:set_resp_header("Connection", "close", ReqData),
    {"", ReqData2, State}.
