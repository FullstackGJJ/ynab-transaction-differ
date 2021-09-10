## BankAccountTransactionParser domain

This domain is responsible for parsing and returning a bank account transaction from a delimited string

Mental Models:

- A bank account transaction is a mental model that holds information about date, description, debit, and credit
- A delimited row is a string that contains information about date, description, debit, and credit separated by a delimiter
- A row header map is a mapping for what positions in a delimited row do date, description, debit, and credit exist
- An empty cell represents when there is nothing between delimiters
- A row parsing error is a mental model that contains information about reason for rejecting the input

The rules of thumb are:

- An empty cell can never occupy the position of a date
- Either debit or credit can exist in a delimited row but never both together absent or present at the same time

An expert BankAccountTransactionParser should always be able to:

- Correctly reject bad input that break the rules of thumb
- Be able to return a correct bank account transaction when given a valid delimited row

An expert BankAccountTransactionParser under the right conditions can:
