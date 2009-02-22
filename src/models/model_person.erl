%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Model for person data.  Interfaces between zophrenic and the database.

-module(model_person).
-author("Marc Worrell <marc@worrell.nl>").

-export([cookie_to_person/1, associate_cookie/2, associate_cookie/3]).

%% @spec cookie_to_person(CookieId) -> undefined | {PersonId, AutoLogon}
%% @doc Map person cookie id to a person id.  Return undefined when it is an unknown user
cookie_to_person(undefined) ->
    undefined;
cookie_to_person(CookieId) ->
    zp_depcache:get({person_cookie, CookieId}).


%% @spec associate_cookie(CookieId, PersonId) -> void()
%% @doc Save the association between a cookie id and a person id, do not set the autologon flag
associate_cookie(CookieId, PersonId) ->
    associate_cookie(CookieId, PersonId, false).

%% @spec associate_cookie(CookieId, PersonId, AutoLogon) -> void()
%% @doc Save the association between a cookie id and a person id, set or do not set the autologon flag
%% @todo Save the person cookie to the db when the user is not anonymous
associate_cookie(CookieId, PersonId, AutoLogon) ->
    AutoLogon1 = zp_utils:is_true(AutoLogon),
    %% @todo: ?set a 'protected' flag on this depcache entry to prevent becoming a gc victim?
    zp_depcache:set({person_cookie, CookieId}, {PersonId, AutoLogon1}, 7200).
