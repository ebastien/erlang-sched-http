%%-*- mode: erlang -*-
{application, wm_sched,
 [
  {description, "wm_sched"},
  {vsn, "1"},
  {modules, [
             wm_sched,
             wm_sched_app,
             wm_sched_sup,
             wm_sched_tools,
             wm_sched_allpaths,
             wm_sched_trips,
             wm_sched_alltrips,
             wm_sched_airlines,
             wm_sched_setup
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  crypto,
                  mochiweb,
                  webmachine
                 ]},
  {mod, { wm_sched_app, []}},
  {env, []}
 ]}.
