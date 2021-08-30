module TransactionDiffer.DomainModels where

import Date.Time.Calendar.Days

type Date = Day

type Amount = String

type Merchant = String

data Transaction = Transaction { 
    date::Date, 
    amount::Amount,
    merchange::Merchant
} deriving (Eq, Show)
