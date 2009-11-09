{application, idp_proxy,
 [{description, "idp_proxy"},
  {vsn, "0.0.1"},
  {modules, [
    idp_proxy,
    idp_proxy_app,
    idp_proxy_sup,
    idp_proxy_web,
    idp_proxy_deps
  ]},
  {registered, []},
  {mod, {idp_proxy_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
