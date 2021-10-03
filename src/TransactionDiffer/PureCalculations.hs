module TransactionDiffer.PureCalculations where

import qualified TransactionDiffer.DomainModels as DM
import qualified TransactionDiffer.DomainRules as DR
import qualified TransactionDiffer.InternalToCalculations as I

-----------------Function Declarations-----------------
findTransactionDiffs :: [DM.Transaction] -> [DM.Transaction] -> DM.TransactionsDiff

----------------Function Implementations----------------
findTransactionDiffs referenceList outOfSyncList = let
    verifiedTransactions = I.determineVerifiedTransactions referenceList outOfSyncList
    missingTransactions = I.determineMissingTransactions referenceList outOfSyncList
    extraTransactions = I.determineExtraTransactions referenceList outOfSyncList
    in DM.TransactionsDiff { DM.tdVerified = verifiedTransactions
                           , DM.tdExtra = extraTransactions
                           , DM.tdMissing = missingTransactions
                           }
