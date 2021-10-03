module TransactionDiffer.DomainModels where

import Data.Time

type Date = UTCTime

type Amount = Float

type Merchant = String

data Transaction = Transaction { 
    date::Date, 
    amount::Amount,
    merchant::Merchant
} deriving (Eq, Show)

data TransactionsDiff = TransactionsDiff {
    tdVerified::[Transaction],
    tdExtra::[Transaction],
    tdMissing::[Transaction]
} deriving (Eq, Show)
