module PureCalculations where

import TransactionDiffer.DomainModels as TD_DM
import TransactionDiffer.PureCalculations as TD_PC

import qualified DomainModels as DM
import qualified DomainRules as DR

import Data.Map
import Data.List

-----------------Function Declarations-----------------
calculateTransactionDiffs :: DM.DateMappedTransactionCollection -> DM.DateMappedTransactionCollection -> DM.DateMappedTransactionDiffs

----------------Function Implementations----------------
calculateTransactionDiffs referenceTransactions outOfSyncTransactions = let
    needsDateReconcilation = not (DR.containsMatchingDates referenceTransactions outOfSyncTransactions)
    referenceTransactions' = if needsDateReconcilation then DR.populateMissingDates outOfSyncTransactions referenceTransactions else referenceTransactions
    outOfSyncTransactions' = if needsDateReconcilation then DR.populateMissingDates referenceTransactions outOfSyncTransactions else outOfSyncTransactions
    referenceTransactionsKeys = keys referenceTransactions'
    in Data.List.foldl 
        (\acc key -> Data.Map.insert key (TD_PC.findTransactionDiffs (referenceTransactions' ! key) (outOfSyncTransactions' ! key)) acc)
        (fromList [])
        referenceTransactionsKeys
