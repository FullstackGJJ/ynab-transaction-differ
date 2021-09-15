## YnabTransactionDiffer domain

This domain is responsible for finding the transaction diffs between account transaction records stored on ynab api and a list of bank account transactions that originate from csv file

Mental Models:

- A transaction is a mental model that holds information about date, amount, merchant
- A date sorted transaction collection is a mental representation of various transactions aggregated by their transaction date
- A csv line is a line of text that represent columns of data separated by commas
- A bank account transaction is a mental model that holds date, description, debit, and credit
- A row header map is a description of which columns on a csv file represents date, description, debit, and credit for a bank account transaction

The rules of thumb are:

- When diffing two date sorted transaction collections, they both must have matching dates with each other, even if one collection has no transaction for a date but the other one does

An expert YnabTransactionDiffer should always be able to:

- Return correct transaction diffs when given YNAB account transactions and bank account transaction history csv lines

An expert YnabTransactionDiffer under the right conditions can:

- Return correct transaction diffs when given valid YNAB API credentials and bank account transaction history csv lines
