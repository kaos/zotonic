%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Model for resource data. Interfaces between zophrenic, templates and the database.

-module(m_rsc).
-author("Marc Worrell <marc@worrell.nl>").

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
p(#rsc{id=Id}, Predicate, Context) -> 
	"value";
p(Id, Predicate, Context) when is_integer(Id) ->
	p(#rsc{id=Id}, Predicate, Context);
p({rsc_list, [R|_]}, Predicate, Context) ->
	p(R, Predicate, Context);
p({rsc_list, []}, _Predicate, _Context) ->
	undefined;
p(Value, _Predicate, _Context) -> 
	Value.

%% Return a list of all edge predicates of this resource
op(Id, Context) -> [].

%% Used for dereferencing object edges inside template expressions
o(Id, _Context) ->
	fun(P, Context) -> o(Id, P, Context) end.

o(Id, Predicate, Context) ->
	{rsc_list, []}.
	
o(Id, Predicate, Index, Context) ->
	#rsc{id=undefined}.
	
%% Return a list of all edge predicates to this resource
sp(Id, Context) -> 
	[].

%% Used for dereferencing subject edges inside template expressions
s(Id, _Context) ->
	fun(P, Context) -> s(Id, P, Context) end.

s(Id, Predicate, Context) ->
	{rsc_list, []}.
	
s(Id, Predicate, Index, Context) ->
	#rsc{id=undefined}.

media(Id, Context) -> 
	[].

media(Id, Index, Context) ->
	undefined. % | MediaPropList.
