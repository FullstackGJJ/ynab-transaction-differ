## TransactionDiffer domain

This domain is responsible for diffing a reference list of transactions against a list that is out of sync. 

A transaction is a mental model that holds information about date, amount, merchant

The rules of thumb are:

- If a transaction in the reference list has the same amount as a transaction in the out of sync list, then that transaction is "verified"
- If a transaction in the reference list does not exist in the out of sync list, then that transaction is considered "missing" from the out of sync list
- If a transaction in the out of sync list does not exist in the reference list, then that transaction is considered "extra" in the out of sync list
- Assume that both transaction lists are for a single day (multiple dates can complicate this domain and is how I naturally diff transaction in real life)

An expert TransactionDiffer should always be able to:

- Take two transaction lists, one referenceList, one outOfSyncList, and accurately give back verifiedList, missingList, and extraList

An expert TransactionDiffer under the right conditions can:
