-module(accounts).
-include("../include/records.hrl").
-compile(export_all).

create(Number, Amount, ReserveAmount) ->
  case check(Number) of
    {account_does_not_exist} ->
      Obj = new_record(Number, Amount, ReserveAmount),
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
    true ->
      case accounts:deposit(A, T#transaction.amount) of
        {ok, Record} -> Record;
        Other -> Other
      end;
    false ->
      case accounts:reserve(A, T#transaction.amount) of
        {ok, Record} -> Record;
        Other -> Other
      end
  end.

check(Number) ->
  Obj = {account, '_', Number, '_', '_'},
  F = fun() -> mnesia:match_object(Obj) end,
  case mnesia:transaction(F) of
    {atomic, []} -> {account_does_not_exist};
    {atomic, [Record]} -> {account_exist, Record}
  end.

has_amount(A = #account{}, Amount) when A#account.amount >= abs(Amount) -> true;
has_amount(_A = #account{}, _Amount) -> false.

has_reserve_amount(A = #account{}, Amount) when A#account.reserve_amount >= abs(Amount) -> true;
has_reserve_amount(_A = #account{}, _Amount) -> false.

new_record(Number, Amount, ReserveAmount) ->
  #account{id = erlang:make_ref(), number = Number, amount = Amount, reserve_amount = ReserveAmount}.

deposit(A = #account{}, Amount) when Amount >= 0 ->
  OldAmount = A#account.amount,
  NewAmount = OldAmount + Amount,
  Record = A#account{amount=NewAmount},
  {ok, Record};

deposit(A = #account{}, Amount) ->
  case has_amount(A, Amount) of
    true ->
      OldAmount = A#account.amount,
      NewAmount = OldAmount + Amount,
      Record = A#account{amount=NewAmount},
      {ok, Record};
    false -> {error, insufficient_funds}
  end.

reserve(A = #account{}, Amount) when Amount >= 0 ->
  case deposit(A, -Amount) of
    {ok, Record} ->
      OldReserveAmount = Record#account.reserve_amount,
      NewReserveAmount = OldReserveAmount + Amount,
      NewRecord = Record#account{reserve_amount = NewReserveAmount},
      {ok, NewRecord};
    Other -> Other
  end;

reserve(A = #account{}, Amount) ->
  case has_reserve_amount(A, Amount) of
    true ->
      {ok, Record} = deposit(A, abs(Amount)),
      OldReserveAmount = Record#account.reserve_amount,
      NewReserveAmount = OldReserveAmount + Amount,
      NewRecord = Record#account{reserve_amount = NewReserveAmount},
      {ok, NewRecord};
    false -> {error, insufficient_reserve_funds}
  end.
