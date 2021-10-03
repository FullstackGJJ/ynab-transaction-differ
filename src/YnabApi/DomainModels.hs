{-# LANGUAGE DeriveGeneric #-}

module YnabApi.DomainModels where

import GHC.Generics
import Data.Time

type Date = UTCTime

type Amount = Float

type PayeeName = String

data TransactionDetail = TransactionDetail { 
    date::Date, 
    amount::Amount,
    payeeName::PayeeName
} deriving (Eq, Show, Generic)

data TransactionsData = TransactionsData {
    transactions::[TransactionDetail]
} deriving (Eq, Show, Generic)

data TransactionResponse = TransactionResponse {
    trData::TransactionsData
} deriving (Eq, Show, Generic)
