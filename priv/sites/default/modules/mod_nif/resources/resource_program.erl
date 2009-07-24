%% @author Marc Worrell
%% @copyright (c) 2009 Marc Worrell <marc@worrell.nl>
%% @date 2009-07-23
%%
%% @doc Resource for the program page, filters on genre and date.

-module(resource_program).

-export([
    resource_exists/2,
    is_authorized/2,
    event/2
]).

-include_lib("resource_html.hrl").

%% @doc Check if the id in the request (or dispatch conf) exists.
resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    try
        ?WM_REPLY(m_rsc:exists(get_id(Context1), Context1), Context1)
    catch
        _:_ -> ?WM_REPLY(false, Context1)
    end.


%% @doc Check if the current user is allowed to view the resource. 
is_authorized(ReqData, Context) ->
    z_auth:wm_is_authorized(false, visible, get_id(Context), ReqData, Context).


%% @doc Show the page.
html(Context) ->
    {Day, GenreIds} = case z_context:get_visitor(program_filter, Context) of
        undefined -> {default_day(), undefined};
        SavedFilters -> SavedFilters
    end,
	Id = get_id(Context),
    Html = z_template:render("program.tpl", [ {id, Id}, {day, Day}, {genre, GenreIds} | z_context:get_all(Context) ], Context),
	z_context:output(Html, Context).


%% @doc Fixed id, translate to numeric.
%% @spec get_id(Context) -> int()
get_id(Context) ->
    m_rsc:name_to_id_check(page_program, Context).
    

%% @doc Selections of the program list are posted here.
event({submit, {search, Props}, _TriggerId, _TargetId}, Context) ->
    Q = z_context:get_q_all(Context),
    Genres = lists:filter(
                    fun ({_,[]}) -> false;
                        ({A, _}) -> z_utils:only_digits(A)
                    end, Q),
    GenreIds = [ list_to_integer(N) || {N, _} <- Genres ],
    Day = case find_day(Q) of
        undefined -> proplists:get_value(day, Props);
        D -> D
    end,
    z_context:set_visitor(program_filter, {Day, GenreIds}, Context),
    {Html, Context1} = z_template:render_to_iolist("_program.tpl", [{day, Day}, {genre, GenreIds}], Context),
    z_render:update("the-program", Html, Context1).


    find_day([]) -> 
        undefined; 
    find_day([{[$d|Nr], _} | Rest]) ->
        case z_utils:only_digits(Nr) of
            true -> list_to_integer(Nr);
            false -> find_day(Rest)
        end;
    find_day([_|Rest]) ->
        find_day(Rest).


%% @doc Return the default day for showing the program
default_day() ->
    case calendar:local_time() of
        {{Y,M,D}, _} when Y == 2009, M == 9 ->
            case D of
                N when N >= 10, N =< 13 -> N;
                N when N >= 17, N =< 20 -> N;
                N when N > 13, N < 17 -> 17;
                _ -> 10
            end;
        _ -> 
            10
    end.
