module Library 
    (
    -- Main Interface
      diffWithBankTransactions

    -- Types
    , DM.YnabApiParameters (..)
    , BTP_DM.RowHeaderMap (..)
    ) where

import qualified DomainModels as DM
import qualified DomainRules as DR
import qualified PureCalculations as PC
import qualified ImpureActions as IA

import qualified YnabApi.DomainModels as YA_DM
import qualified YnabApi.ImpureActions as YA_IA

import TransactionDiffer.DomainModels as TD_DM

import qualified BankAccountTransactionParser.DomainModels as BTP_DM
import qualified BankAccountTransactionParser.PureCalculations as BTP_PC

import Data.List
import Data.Map

diffWithBankTransactions :: DM.YnabApiParameters -> [DM.CsvLine] -> BTP_DM.RowHeaderMap -> IO (Either String DM.DateMappedTransactionDiffs)
diffWithBankTransactions ynabApiParameters csvLines rowHeaderMap = IA.calculateDiffFromYnabApiAndCsvLines ynabApiParameters csvLines rowHeaderMap 
