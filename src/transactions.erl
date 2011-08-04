-module(transactions).
-include("../include/records.hrl").
-compile(export_all).

create(AccountNumber, Amount, Confirm) ->
  case accounts:check(AccountNumber) of
    {account_exist, A} ->
      Transaction = new_record(AccountNumber, Amount, Confirm),
      Account = case Confirm of
        true -> accounts:change_record(A, amount, Amount);
        false -> accounts:change_record(A, reserve_amount, Amount)
      end,
      process_transaction(Account, Transaction);
    Result -> Result
  end.

process_transaction(A = #account{}, T = #transaction{}) ->
  F = fun() ->
    mnesia:write(A),
    mnesia:write(T)
  end,
  mnesia:transaction(F).

all(AccountNumber) ->
  case accounts:check(AccountNumber) of
    {account_exist, _Record} ->
      Obj = {transaction, '_', AccountNumber, '_', '_'},
      F = fun() -> mnesia:match_object(Obj) end,
      {atomic, Result} = mnesia:transaction(F),
      Result;
    Other -> Other
  end.

new_record(AccountNumber, Amount, Confirm) ->
  #transaction{id = erlang:make_ref(), account_number = AccountNumber, amount = Amount, confirm = Confirm}.
