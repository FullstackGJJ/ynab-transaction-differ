module InternalToActions where

import qualified DomainModels as DM
import qualified DomainRules as DR
import qualified PureCalculations as PC

import qualified YnabApi.DomainModels as YA_DM
import qualified YnabApi.ImpureActions as YA_IA

import TransactionDiffer.DomainModels as TD_DM

import qualified BankAccountTransactionParser.DomainModels as BTP_DM
import qualified BankAccountTransactionParser.PureCalculations as BTP_PC

import Data.Either
import Data.List
import Data.Map
import Data.Set
import Data.Time
import Data.Time.Format

-----------------Function Declarations-----------------


----------------Function Implementations----------------
convertCsvLinesToBankAccountTransactionAttempts csvLines rowHeaderMap = let 
    lineAttempts = Data.List.map (\x -> BTP_PC.createBankAccountTransaction x rowHeaderMap) csvLines
    anyParsingError = Data.List.any (\x -> case x of (Left error) -> True; (Right transaction) -> False) lineAttempts
    in if anyParsingError 
       then (Left "Error in parsing input")
       else (Right (Data.List.map (\x -> fromRight emptyBankAccountTransaction x) lineAttempts))

emptyBankAccountTransaction = BTP_DM.BankAccountTransaction {
    BTP_DM.date = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/01/1970" :: UTCTime,
    BTP_DM.description = "",
    BTP_DM.debit = 0,
    BTP_DM.credit = 0
}

getYnabAggregateTransactions transactionPossibility =
    case transactionPossibility of (Left error) -> (Left error)
                                   (Right result) -> (Right (aggregateYnabTransactionDetails result))

aggregateYnabTransactionDetails transactionDetailList = let
    dates = Data.Set.fromList (Data.List.map (\x -> (YA_DM.date x)) transactionDetailList)
    in Data.List.foldl
        (\acc x -> Data.Map.insert x (Data.List.map (convertYnabToTransaction) (Data.List.filter (\y -> (YA_DM.date y) == x) transactionDetailList)) acc)
        (Data.Map.fromList [])
        (Data.Set.toList dates)

convertYnabToTransaction transactionDetail = TD_DM.Transaction { TD_DM.date = (YA_DM.date transactionDetail)
                                                               , TD_DM.amount = (YA_DM.amount transactionDetail)
                                                               , TD_DM.merchant = (YA_DM.payeeName transactionDetail)
                                                               }

getBankAccountAggregateTransactions transactionPossibility = case transactionPossibility of (Left error) -> (Left error)
                                                                                            (Right result) -> (Right (aggregateBankAccountTransactions result))

aggregateBankAccountTransactions :: [BTP_DM.BankAccountTransaction] -> DM.DateMappedTransactionCollection
aggregateBankAccountTransactions bankAccountTransactionList = let
    dates = Data.Set.fromList (Data.List.map (\x -> (BTP_DM.date x)) bankAccountTransactionList)
    in Data.List.foldl
        (\acc x -> Data.Map.insert x (Data.List.map (convertBankTransactionsToTransactions) (Data.List.filter (\y -> (BTP_DM.date y) == x) bankAccountTransactionList)) acc)
        (Data.Map.fromList [])
        (Data.Set.toList dates)

convertBankTransactionsToTransactions :: BTP_DM.BankAccountTransaction -> TD_DM.Transaction
convertBankTransactionsToTransactions bankAccountTransaction = let
    debit = (BTP_DM.debit bankAccountTransaction)
    credit = (BTP_DM.credit bankAccountTransaction)
    amountDetermination = if debit == 0 then negate credit else negate debit
    in TD_DM.Transaction { TD_DM.date = (BTP_DM.date bankAccountTransaction)
                         , TD_DM.amount = amountDetermination
                         , TD_DM.merchant = (BTP_DM.description bankAccountTransaction)
                         }
