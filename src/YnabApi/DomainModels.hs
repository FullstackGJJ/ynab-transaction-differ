{-# LANGUAGE DeriveGeneric #-}

module YnabApi.DomainModels where

import GHC.Generics

type Date = String

type Amount = Float

type PayeeId = String

data TransactionDetail = TransactionDetail { 
    date::Date, 
    amount::Amount,
    payeeId::PayeeId
} deriving (Eq, Show, Generic)

data TransactionsData = TransactionsData {
    transactions::[TransactionDetail]
} deriving (Eq, Show, Generic)

data TransactionResponse = TransactionResponse {
    trData::TransactionsData
} deriving (Eq, Show, Generic)
