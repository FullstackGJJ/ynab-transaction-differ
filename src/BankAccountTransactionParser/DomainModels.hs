module BankAccountTransactionParser.DomainModels where

type Date = String

type Description = String

type Debit = Float

type Credit = Float

data BankAccountTransaction = BankAccountTransaction {
    date::Date,
    description::Description,
    debit::Debit,
    credit::Credit
} deriving (Eq, Show)
