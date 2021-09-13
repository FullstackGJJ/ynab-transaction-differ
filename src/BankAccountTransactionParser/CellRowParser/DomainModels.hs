module BankAccountTransactionParser.CellRowParser.DomainModels where

type DelimitedRow = String

type CellRow = [Cell]

data Cell = Empty
          | Filled String
