## BankAccountTransactionParser domain

This domain is responsible for parsing and returning a bank account transaction from a delimited string

Mental Models:

- A bank account transaction is a mental model that holds information about date, description, debit, and credit
- A row header map is a mental model that holds information regarding positions in a delimited row where date, description, debit, and credit exist
- A cell is a mental model that holds information about a column
- A cell row is a list of cells
- An empty cell represents when there is nothing between delimiters
- A filled cell represents where there is something between delimiters

The rules of thumb are:

- If a row header map entry references a column number that is out of bounds then the configuration and data are incompatible
- An empty cell can never occupy the position of a date
- Either debit or credit can be empty cells but never both together empty or filled at the same time

An expert BankAccountTransactionParser should always be able to:

- Return a correct bank account transaction when given a valid delimited row

An expert BankAccountTransactionParser under the right conditions can:
