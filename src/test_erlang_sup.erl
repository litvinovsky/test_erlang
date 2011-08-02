-module(test_erlang_sup).
-behaviour(supervisor).
-export([init/1, start_link/0]).

start_link() ->
  supervisor:start_link({local,?MODULE}, ?MODULE, []).

init(_Args) ->
  Worker = {w1, {test_erlang_server, start_link, []}, permanent, 5000, worker, [test_erlang_server]},
  {ok, {{one_for_one, 5, 60}, [Worker]}}.
