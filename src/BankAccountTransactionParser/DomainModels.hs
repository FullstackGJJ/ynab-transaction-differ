module BankAccountTransactionParser.DomainModels where

type Date = String

type Description = String

type Debit = Float

type Credit = Float

type DelimitedRow = String

data Cell a = Empty
            | Filled a

data BankAccountTransaction = BankAccountTransaction {
    date::Date,
    description::Description,
    debit::Debit,
    credit::Credit
} deriving (Eq, Show)

data RowHeaderMap = RowHeaderMap {
    rhmDate::Integer,
    rhmDescription::Integer,
    rhmDebit::Integer,
    rhmCredit::Integer
} deriving (Eq, Show)

data RowParsingError = RowParsingError {
    reason::String
}
