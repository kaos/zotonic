%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% @date 2009-09-28
%%
%% @doc Simple service for testing the API handler.

-module(service_test_hello).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([process_get/2]).

process_get(ReqData, Context) ->
    'Hello, world!'.
