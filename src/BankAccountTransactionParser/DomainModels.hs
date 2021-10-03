module BankAccountTransactionParser.DomainModels where

import Data.Time

type Date = UTCTime

type Description = String

type Debit = Float

type Credit = Float

data BankAccountTransaction = BankAccountTransaction {
    date::Date,
    description::Description,
    debit::Debit,
    credit::Credit
} deriving (Eq, Show)

data RowHeaderMap = RowHeaderMap {
    rhmDate::Int,
    rhmDescription::Int,
    rhmDebit::Int,
    rhmCredit::Int
} deriving (Eq, Show)

data RowParsingError = RowParsingError {
    reason::String
} deriving (Eq, Show)
