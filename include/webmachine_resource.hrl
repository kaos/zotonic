-behaviour(webmachine_resource).
-export([start_link/1]).
-export([ping/2]).

-include_lib("webmachine.hrl").

start_link(Args) ->
    webmachine_resource:start_link(?MODULE, [Args]).

ping(_ReqProps, State) ->
    {pong, State}.

