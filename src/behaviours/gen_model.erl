%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-12
%%
%% @doc Model behaviour

-module(gen_model).
-author("Marc Worrell <marc@worrell.nl").

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {m_find_value, 3},
        {m_value, 2},
        {m_to_list, 2}
     ];
behaviour_info(_Other) ->
    undefined.
