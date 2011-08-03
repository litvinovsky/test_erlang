-module(test_erlang_app).
-behaviour(application).
-export([start/2, stop/1, replace_for_dirty_update_counter/0]).

start(normal, _Args) ->
  test_erlang_sup:start_link().

stop(_State) -> ok.

replace_for_dirty_update_counter() ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  random:uniform(1000000000).
