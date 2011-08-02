-module(test_erlang_server).
-behaviour(gen_server).

-export([start_link/0, reserve_amount/3, confirm_transaction/2,
cancel_transaction/2, charge_amount/3, refill_amount/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
terminate/2]).

% api
start_link() ->
  gen_server:start_link(?MODULE, [], []).

reserve_amount(Pid, AccountNumber, Amount) ->
  gen_server:call(Pid, {reserve_amount, AccountNumber, Amount}).

confirm_transaction(Pid, TransactionId) ->
  gen_server:call(Pid, {confirm_transaction, TransactionId}).

cancel_transaction(Pid, TransactionId) ->
  gen_server:call(Pid, {cancel_transaction, TransactionId}).

charge_amount(Pid, AccountNumber, Amount) ->
  gen_server:call(Pid, {charge_amount, AccountNumber, Amount}).

refill_amount(Pid, AccountNumber, Amount) ->
  gen_server:call(Pid, {refill_amount, AccountNumber, Amount}).

init(_Args) ->
  {ok, []}.

% reserve amount
handle_call({reserve_amount, _AccountNumber, _Amount}, _From, _State) ->
  io:format("reserve amount"),
  {reply, ok, _State};

% confirm transaction
handle_call({confirm_transaction, _TransactionId}, _From, _State) ->
  io:format("confirm transaction"),
  {reply, ok, _State};

% cancel transaction
handle_call({cancel_transaction, _TransactionId}, _From, _State) ->
  io:format("cancel transaction"),
  {reply, ok, _State};

% charge amount
handle_call({charge_amount, _AccountNumber, _Amount}, _From, _State) ->
  io:format("charge amount"),
  {reply, ok, _State};

% refill amount
handle_call({refill_amount, _AccountNumber, _Amount}, _From, _State) ->
  io:format("refill amount"),
  {reply, ok, _State};

% unknown request
handle_call(_Request, _From, _State) ->
  io:format("~p~n", [_Request]),
  {reply, ok, _State}.

handle_cast(_Request, _State) ->
  {noreply, _State}.

handle_info(_Msg, _State) ->
  {noreply, _State}.

terminate(_Status, _State) ->
  ok.

code_change(_OldVsn, _State, _Extra) ->
  {ok, _State}.
