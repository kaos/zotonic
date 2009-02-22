%% @doc definition for webmachine

-define(REQ(RP), proplists:get_value(req, RP)).
-define(PATH(RP), proplists:get_value(path, RP)).
