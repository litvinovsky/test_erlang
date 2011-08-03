-module(transactions).
-include("../include/records.hrl").
-compile(export_all).

create(AccountNumber, Amount, Confirm) ->
  case accounts:check(AccountNumber) of
    {account_exist, _Record} ->
      % It is harcoded, because I have some trubles with standard function.
      % I will look into this problem
      Id = test_erlang_app:replace_for_dirty_update_counter(),

      Obj = new_transaction_record(Id, AccountNumber, Amount, Confirm),
      F = fun() -> mnesia:write(Obj) end,
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

new_transaction_record(Id, AccountNumber, Amount, Confirm) ->
  #transaction{id = Id, account_number = AccountNumber, amount = Amount, confirm = Confirm}.
