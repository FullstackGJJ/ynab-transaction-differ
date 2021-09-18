module TransactionDiffer.DomainModels where

type Date = String

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
