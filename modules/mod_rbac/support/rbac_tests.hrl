%% users
-define(USR1, 1). %% a.k.a. admin
-define(USR2, 2).

%% predicates
-define(PREDICATE, 50).
-define(RBAC_ROLE, 51).
-define(RBAC_DOMAIN, 52).

%% domain resources
-define(DOMAIN1, 101).
-define(DOMAIN2, 102).

%% role resources
-define(ROLE1, 201).
-define(ROLE2, 202).
-define(ROLE3, 203).
-define(ROLE4, 204).
-define(ROLE5, 205).
-define(ROLE6, 206).
-define(ROLE7, 207).
-define(ROLE8, 208).

%% operation resources
-define(OP1, 301).
-define(OP2, 302).

%% content resources
-define(RSC1, 401).
-define(RSC2, 402).

%% edges
-define(DOMAIN1_ROLES, [?ROLE1, ?ROLE2, ?ROLE3]).
-define(DOMAIN2_ROLES, [?ROLE3, ?ROLE4, ?ROLE5]).
-define(DOMAIN2_ROLES_IMPLICIT, [?ROLE6, ?ROLE7, ?ROLE8]).

-define(RSC_DOMAINS, [{?RSC1, ?DOMAIN1}, {?RSC2, ?DOMAIN2}]).
-define(DOMAIN_ROLES, [{?DOMAIN1, ?DOMAIN1_ROLES}, {?DOMAIN2, ?DOMAIN2_ROLES}]).

-define(ROLE_HIERARCHY, [{?ROLE5, [?ROLE6, ?ROLE7]}, {?ROLE7, [?ROLE8]}]).
