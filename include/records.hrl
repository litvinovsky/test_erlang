-record(account, {id, number, amount = 0, reserve_amount = 0}).
-record(transaction, {id, account_number, amount, confirm}).
