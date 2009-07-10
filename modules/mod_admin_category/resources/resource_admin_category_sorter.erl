%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Creates an editable overview of all categories.

-module(resource_admin_category_sorter).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2,
    event/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    zp_auth:wm_is_authorized(ReqData, Context).


html(Context) ->
	Html = zp_template:render("admin_category_sorter.tpl", [{page_admin_category_sorter, true}, {delegate, ?MODULE}], Context),
	zp_context:output(Html, Context).

%% @doc Handle the drop of a dragged category. The category can be dropped on another category (making it a sub-category) or on
%% a separator between two categories.  The first will make the category a sub category, the second will move the category to 
%% the spot of the separator, which can be before another category or at the "end" spot.
event({drop, {dragdrop, DragTag, _DragDelegate, _DragId}, {dragdrop, DropTag, _DropDelegate, _DropId}}, Context) ->
    DragId = case DragTag of 
        "t-"++T -> list_to_integer(T) 
    end,
    Result = case DropTag of
        "t-"++B -> m_category:move_below(DragId, list_to_integer(B), Context);
        "b-"++B -> m_category:move_before(DragId, list_to_integer(B), Context);
        "end" ->   m_category:move_end(DragId, Context)
    end,
    case Result of
       ok ->
           {Html, Context1} = zp_template:render_to_iolist("_admin_category_sorter.tpl", [{delegate, ?MODULE}], Context),
           zp_render:update("category-sorter", Html, Context1);
       {error, cycle} ->
           zp_render:wire({growl, [{text, "Can not make a category a sub category of itself."}, {type, "error"}]}, Context);
       {error, eacces} ->
           zp_render:wire({growl, [{text, "You are not allowed to change categories."}, {type, "error"}]}, Context)
    end;
        
event(_Event, Context) ->
    Context.

