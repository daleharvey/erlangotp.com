%% -*- erlang -*-

{application, erlangotp,
 [
  {description, ""},
  {vsn, "1"},   
  {modules,
   [
    erlangotp_app,
    erlangotp_sup
   ]},
  {registered, []},
  {applications,
   [
    kernel,
    stdlib,
    sasl
   ]},
  {mod, { erlangotp_app, []}},
  {env, []}
 ]}.
