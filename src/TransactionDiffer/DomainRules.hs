module TransactionDiffer.DomainRules where

import Data.List
import Data.Maybe

import qualified TransactionDiffer.DomainModels as DM

-----------------Function Declarations-----------------
isVerified :: DM.Transaction -> [DM.Transaction] -> Bool
isMissing :: DM.Transaction -> [DM.Transaction] -> Bool
isExtra :: DM.Transaction -> [DM.Transaction] -> Bool

----------------Function Implementations----------------
isVerified transaction transactionList =
    let searchResult = find (\x -> (DM.amount x) == (DM.amount transaction)) transactionList
    in case searchResult of 
        Just _ -> True
        Nothing -> False

isMissing transaction transactionList =
    let searchResult = find (\x -> (DM.amount x) == (DM.amount transaction)) transactionList
    in case searchResult of 
        Just _ -> False
        Nothing -> True

isExtra transaction transactionList = isMissing transaction transactionList
