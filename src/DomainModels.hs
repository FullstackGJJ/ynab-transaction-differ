module DomainModels where

import TransactionDiffer.DomainModels as TD_DM
import BankAccountTransactionParser.DomainModels as BTP_DM
import BankAccountTransactionParser.CellRowParser.DomainModels as BTP_CRP_DM

import Data.Map (Map)

type DateMappedTransactionCollection = Map TD_DM.Date [TD_DM.Transaction]

type DateMappedTransactionDiffs = Map TD_DM.Date TD_DM.TransactionsDiff

type CsvLine = BTP_CRP_DM.DelimitedRow

data YnabApiParameters = YnabApiParameters {
    bearerToken::String,
    budgetId::String,
    accountId::String,
    sinceDate::String
} deriving (Eq, Show)
