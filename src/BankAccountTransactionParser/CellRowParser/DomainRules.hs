module BankAccountTransactionParser.CellRowParser.DomainRules where

import BankAccountTransactionParser.CellRowParser.DomainModels as DM

import Data.List

-----------------Function Declarations-----------------
containsDelimiter :: String -> DM.DelimitedRow -> Bool

----------------Function Implementations----------------
containsDelimiter delimiter delimitedRow = isInfixOf delimiter delimitedRow
