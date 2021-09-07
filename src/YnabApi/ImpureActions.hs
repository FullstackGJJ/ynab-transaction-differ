module YnabApi.ImpureActions where

import qualified YnabApi.DomainModels as DM
import qualified YnabApi.DomainRules as DR
import qualified YnabApi.InternalToActions as I

-----------------Function Declarations-----------------
readTransactionsM :: String -> String -> String -> String -> IO (Either String [DM.TransactionDetail])

----------------Function Implementations----------------

readTransactionsM bearerToken budgetId accountId sinceDate = I.getTransactionsFromApiM bearerToken budgetId accountId sinceDate

