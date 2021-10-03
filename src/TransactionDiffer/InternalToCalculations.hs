module TransactionDiffer.InternalToCalculations where

import qualified TransactionDiffer.DomainModels as DM
import qualified TransactionDiffer.DomainRules as DR

-----------------Function Declarations-----------------
determineVerifiedTransactions :: [DM.Transaction] -> [DM.Transaction] -> [DM.Transaction]
determineMissingTransactions :: [DM.Transaction] -> [DM.Transaction] -> [DM.Transaction]
determineExtraTransactions :: [DM.Transaction] -> [DM.Transaction] -> [DM.Transaction]

----------------Function Implementations----------------
determineVerifiedTransactions referenceList [] = []
determineVerifiedTransactions referenceList outOfSyncList = 
    let (verifiedList, _) = foldl (checkForVerified) ([], outOfSyncList) referenceList
    in verifiedList

determineMissingTransactions referenceList [] = referenceList
determineMissingTransactions referenceList outOfSyncList =
    let (missingList, _) = foldl (checkForMissing) ([], outOfSyncList) referenceList
    in missingList

determineExtraTransactions [] outOfSyncList = outOfSyncList
determineExtraTransactions referenceList [] = []
determineExtraTransactions referenceList outOfSyncList =
    let (extraList, _) = foldl (checkForMissing) ([], referenceList) outOfSyncList
    in extraList

checkForVerified (acc, []) transaction = (acc, [])
checkForVerified (acc, outOfSyncList) transaction = 
    if DR.isVerified transaction outOfSyncList
    then
        (acc ++ [transaction], removeFirstTransactionItem transaction outOfSyncList)
    else
        (acc, outOfSyncList)

checkForMissing (acc, []) transaction = (acc, [])
checkForMissing (acc, outOfSyncList) transaction =     
    if DR.isMissing transaction outOfSyncList
    then
        (acc ++ [transaction], outOfSyncList)
    else
        (acc, outOfSyncList)

removeFirstTransactionItem _ [] = []
removeFirstTransactionItem x (y:ys) | (DM.amount x) == (DM.amount y) = ys
                                    | otherwise = y : (removeFirstTransactionItem x ys)
