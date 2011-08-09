-module(accounts).
-include("../include/records.hrl").
-compile(export_all).

create(Number) ->
  case check(Number) of
    {account_does_not_exist} ->
      Obj = new_record(Number, 0),
      F = fun() -> mnesia:write(Obj) end,
      mnesia:transaction(F);
    Result -> Result
  end.

all() ->
  Obj = {account, '_', '_', '_', '_'},
  F = fun() -> mnesia:match_object(Obj) end,
  {atomic, Result} = mnesia:transaction(F),
  Result.

apply_transaction(A = #account{}, T = #transaction{}) ->
  case T#transaction.confirm of
    true -> accounts:deposit(A, T#transaction.amount);
    false -> accounts:reserve(A, T#transaction.amount)
  end.

check(Number) ->
  Obj = {account, '_', Number, '_', '_'},
  F = fun() -> mnesia:match_object(Obj) end,
  case mnesia:transaction(F) of
    {atomic, []} -> {account_does_not_exist};
    {atomic, [Record]} -> {account_exist, Record}
  end.

new_record(Number, Amount) ->
  #account{id = erlang:make_ref(), number = Number, amount = Amount}.

deposit(Record, Amount) ->
  OldAmount = Record#account.amount,
  Record#account{amount=OldAmount + Amount}.

reserve(Record, Amount) ->
  OldReserveAmount = Record#account.reserve_amount,
  Record#account{reserve_amount=OldReserveAmount + Amount}.
