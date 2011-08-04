-module(test_erlang_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
  test_erlang_sup:start_link().

stop(_State) -> ok.
