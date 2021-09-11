module BankAccountTransactionParser.DomainModels where

type Date = String

type Description = String

type Debit = Float

type Credit = Float

type DelimitedRow = String

type ParsedRow = [Cell]

data Cell = Empty
          | Filled String

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
}
