%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Model for resource data. Interfaces between zophrenic, templates and the database.

-module(m_rsc).
-author("l Worrell <marc@worrell.nl>").

-export([
	rsc/0,
	exists/1, 
	is_readable/2, is_writeable/2, is_owner/2, is_ingroup/2, is_me/2,
	p/3, 
	op/2, o/2, o/3, o/4,
	sp/2, s/2, s/3, s/4,
	media/2, media/3
]).

-include_lib("zophrenic.hrl").

rsc() -> fun(Id, _Context) -> #rsc{id=Id} end.

exists(Id) -> true.
is_readable(Id, Context) -> true.
is_writeable(Id, Context) -> true.
is_owner(Id, Context) -> true.
is_ingroup(Id, Context) -> true.
is_me(Id, Context) -> true.

%% Perform access control checks, return 'undefined' on an error or permission denial
%% Unknown properties will be checked against the predicates, returns o(Predicate).
p(Id, media, Context) -> media(Id, Context);
p(Id, o, Context)  -> o(Id, Context);
p(Id, s, Context)  -> s(Id, Context);
p(Id, op, Context) -> op(Id, Context);
p(Id, sp, Context) -> sp(Id, Context);

p(#rsc{id=Id}, Predicate, Context) -> 
	proplists:get_value(Predicate, dummy_data:rsc(Id));
p(undefined, _Predicate, _Context) ->
    undefined;
p(Id, Predicate, Context) ->
    p(rid(Id), Predicate, Context).

%% Return a list of all edge predicates of this resource
op(#rsc{id=Id}, Context) ->
    [];
op(undefined, _Context) -> 
    [];
op(Id, Context) ->
    op(rid(Id), Context).

%% Used for dereferencing object edges inside template expressions
o(Id, _Context) ->
	fun(P, Context) -> o(Id, P, Context) end.

%% Return the list of objects with a certain predicate
o(#rsc{id=Id}, Predicate, Context) ->
	{rsc_list, []};
o(undefined, _Predicate, _Context) ->
    {rsc_list, []};
o(Id, Predicate, Context) ->
    o(rid(Id), Predicate, Context).


%% Return the nth object in the predicate list
o(#rsc{id=Id}, Predicate, Index, Context) ->
	#rsc{id=undefined};
o(undefined, _Predicate, _Index, _Context) ->
    undefined;
o(Id, Predicate, Index, Context) ->
    o(rid(Id), Predicate, Index, Context).

	
%% Return a list of all edge predicates to this resource
sp(#rsc{id=Id}, Context) ->
    [];
sp(undefined, _Context) -> 
    [];
sp(Id, Context) ->
    sp(rid(Id), Context).

%% Used for dereferencing subject edges inside template expressions
s(Id, _Context) ->
	fun(P, Context) -> s(Id, P, Context) end.

%% Return the list of subjects with a certain predicate
s(#rsc{id=Id}, Predicate, Context) ->
	{rsc_list, []};
s(undefined, _Predicate, _Context) ->
    {rsc_list, []};
s(Id, Predicate, Context) ->
    s(rid(Id), Predicate, Context).

%% Return the nth object in the predicate list
s(#rsc{id=Id}, Predicate, Index, Context) ->
	#rsc{id=undefined};
s(undefined, _Predicate, _Index, _Context) ->
    undefined;
s(Id, Predicate, Index, Context) ->
    s(rid(Id), Predicate, Index, Context).


%% Return the list of all media attached to the resource
media(#rsc{id=Id}, Context) -> 
	dummy_data:media(Id);
media(undefined, _Context) -> 
	[];
media(Id, Context) -> 
	media(rid(Id), Context).


media(#rsc{id=Id}, Index, Context) ->
	undefined; % | MediaPropList.
media(undefined, _Index, _Context) ->
	undefined;
media(Id, Index, Context) ->
    media(rid(Id), Index, Context).
	
	
%% @doc Fetch a #rsc{} from any input
rid(#rsc{id=Id} = Rsc) ->
    Rsc;
rid(Id) when is_integer(Id) ->
	#rsc{id=Id};
rid({rsc_list, [R|_]}) ->
	R;
rid({rsc_list, []}) ->
	undefined;
rid(undefined) -> 
	undefined.

