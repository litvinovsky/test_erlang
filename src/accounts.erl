-module(accounts).
-include("../include/records.hrl").
-compile(export_all).

create(Number) ->
  case check(Number) of
    {account_does_not_exist} ->
      % It is harcoded, because I have some trubles with standard function.
      % I will look into this problem
      Id = test_erlang_app:replace_for_dirty_update_counter(),

      Obj = new_account_record(Id, Number, 0),
      F = fun() -> mnesia:write(Obj) end,
      mnesia:transaction(F);
    Result -> Result
  end.

all() ->
  Obj = {account, '_', '_', '_'},
  F = fun() -> mnesia:match_object(Obj) end,
  {atomic, Result} = mnesia:transaction(F),
  Result.

check(Number) ->
  Obj = {account, '_', Number, '_'},
  F = fun() -> mnesia:match_object(Obj) end,
  case mnesia:transaction(F) of
    {atomic, []} -> {account_does_not_exist};
    {atomic, [Record]} -> {account_exist, Record}
  end.

new_account_record(Id, Number, Amount) ->
  #account{id = Id, number = Number, amount = Amount}.
