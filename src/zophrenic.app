{application, zophrenic,
 [{description, "zophrenic"},
  {vsn, "0.18"},
  {modules, [
    zophrenic,
    zophrenic_app,
    zophrenic_sup,
    zophrenic_deps,

	zp_dispatcher,
	zp_depcache
  ]},
  {registered, []},
  {mod, {zophrenic_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
