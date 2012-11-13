%% users
-define(USR1, 1). %% a.k.a. admin
-define(USR2, 2).

%% predicates
-define(PREDICATE, 50).
-define(RBAC_DOMAIN_RSC, 51).
-define(RBAC_DOMAIN_ROLE, 52).
-define(RBAC_ROLE_MEMBER, 53).
-define(RBAC_ROLE_OPERATION, 54).

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
-define(OP_update, 301).
-define(OP21, 321).
-define(OP22, 322).
-define(OP81, 381).
-define(OP82, 382).

%% content resources
-define(RSC1, 401).
-define(RSC2, 402).

%% edges
-define(DOMAIN1_ROLES, [?ROLE1, ?ROLE2, ?ROLE3]).
-define(DOMAIN2_ROLES, [?ROLE3, ?ROLE4, ?ROLE5]).
%% implicit roles comes from domain roles referring to these "outside" roles
%% this works recursively, so implicit roles may also refer to other non-domain roles.
-define(DOMAIN2_ROLES_IMPLICIT, [?ROLE6, ?ROLE7, ?ROLE8]).

-define(DOMAIN_RSCS, 
        [
         {?DOMAIN1, [?RSC1]}, 
         {?DOMAIN2, [?RSC2]}
        ]).

-define(DOMAIN_ROLES, 
        [
         {?DOMAIN1, ?DOMAIN1_ROLES},
         {?DOMAIN2, ?DOMAIN2_ROLES}
        ]).

-define(ROLE_HIERARCHY, 
        [
         {?ROLE5, [?ROLE6, ?ROLE7]},
         {?ROLE7, [?ROLE8]}
        ]).


%% other dep cache data
-define(USER_ROLES, 
        [
         {?USR2, [?ROLE1, ?ROLE2, ?ROLE8]}
        ]).

-define(ROLE_OPS, 
        [
         {?ROLE1, [?OP_update]},
         {?ROLE2, [?OP21, ?OP22]}, 
         {?ROLE8, [?OP81, ?OP82]}
        ]).

-define(RSC_NAMES, 
        [
         {?PREDICATE, "predicate"},
         {?RBAC_DOMAIN_RSC, "rbac_domain_rsc"},
         {?RBAC_DOMAIN_ROLE, "rbac_domain_role"},
         {?RBAC_ROLE_MEMBER, "rbac_role_member"},
         {?RBAC_ROLE_OPERATION, "rbac_role_operation"},
         {?OP_update, "update"},
         {?OP21, "OP21"},
         {?OP22, "OP22"},
         {?OP81, "OP81"},
         {?OP82, "OP82"}
        ]).

-define(PREDICATES, 
        [
         ?RBAC_DOMAIN_RSC,
         ?RBAC_DOMAIN_ROLE,
         ?RBAC_ROLE_MEMBER,
         ?RBAC_ROLE_OPERATION
        ]).
