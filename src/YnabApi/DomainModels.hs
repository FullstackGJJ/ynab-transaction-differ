module YnabApi.DomainModels where

type Date = String

type Amount = Float

type PayeeId = String

data TransactionDetail = TransactionDetail { 
    date::Date, 
    amount::Amount,
    payeeId::PayeeId
} deriving (Eq, Show)
