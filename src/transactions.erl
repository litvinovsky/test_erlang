-module(transactions).
-include("../include/records.hrl").
-compile(export_all).

create(AccountNumber, Amount, Confirm) ->
  case accounts:check(AccountNumber) of
    {account_exist, A} ->
      Transaction = new_record(AccountNumber, Amount, Confirm),
      Account = accounts:apply_transaction(A, Transaction),
      F = fun() ->
        mnesia:write(Account),
        mnesia:write(Transaction)
      end,
      mnesia:transaction(F);
    Result -> Result
  end.

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
