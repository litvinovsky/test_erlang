{application, test_erlang,
  [
    {description, "app for interview"},
    {vsn, "0.1.0"},
    {modules, [test_erlang_app, test_erlang_server, test_erlang_supervisor]},
    {mod, {test_erlang_app, []}}
  ]
}.
