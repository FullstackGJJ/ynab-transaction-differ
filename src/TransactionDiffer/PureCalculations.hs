module TransactionDiffer.PureCalculations where

import qualified TransactionDiffer.DomainModels as DM
import qualified TransactionDiffer.DomainRules as DR
import qualified TransactionDiffer.InternalFunctions as IF

-----------------Function Declarations-----------------
findTransactionDiffs :: [DM.Transaction] -> [DM.Transaction] -> ([DM.Transaction], [DM.Transaction], [DM.Transaction])

----------------Function Implementations----------------
findTransactionDiffs referenceList outOfSyncList = 
    let verifiedTransactions = IF.determineVerifiedTransactions referenceList outOfSyncList
        missingTransactions = IF.determineMissingTransactions referenceList outOfSyncList
        extraTransactions = IF.determineExtraTransactions referenceList outOfSyncList
    in (verifiedTransactions, missingTransactions, extraTransactions)
