module ImpureActions where

import qualified DomainModels as DM
import qualified DomainRules as DR
import qualified PureCalculations as PC
import qualified InternalToActions as I

import qualified YnabApi.DomainModels as YA_DM
import qualified YnabApi.ImpureActions as YA_IA

import TransactionDiffer.DomainModels as TD_DM

import qualified BankAccountTransactionParser.DomainModels as BTP_DM
import qualified BankAccountTransactionParser.PureCalculations as BTP_PC

import Data.Either
import Data.List
import Data.Map
import Data.Set

-----------------Function Declarations-----------------
calculateDiffFromYnabApiAndCsvLines :: DM.YnabApiParameters -> [DM.CsvLine] -> BTP_DM.RowHeaderMap -> IO (Either String DM.DateMappedTransactionDiffs)

----------------Function Implementations----------------
calculateDiffFromYnabApiAndCsvLines ynabParameters csvLines rowHeaderMap = do
    readTransactionsAttempt <- YA_IA.readTransactionsM (DM.bearerToken ynabParameters) (DM.budgetId ynabParameters) (DM.accountId ynabParameters) (DM.sinceDate ynabParameters)
    let ynabAggregatedTransactionsAttempt = I.getYnabAggregateTransactions readTransactionsAttempt
    let bankAccountTransactionAttempt = I.convertCsvLinesToBankAccountTransactionAttempts csvLines rowHeaderMap
    let bankAccountAggregatedTransactionsAttempt = I.getBankAccountAggregateTransactions bankAccountTransactionAttempt
    case (ynabAggregatedTransactionsAttempt, bankAccountAggregatedTransactionsAttempt) of 
        (Right ynabTransactions, Right bankTransactions) -> return (Right (PC.calculateTransactionDiffs bankTransactions ynabTransactions))
        (Left error1, Left error2) -> return (Left (error1 ++ error2))
        (_, _) -> return (Left ("Error in calculating diff"))
