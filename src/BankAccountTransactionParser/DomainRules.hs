{-# LANGUAGE OverloadedStrings #-}
module BankAccountTransactionParser.DomainRules where

import qualified BankAccountTransactionParser.DomainModels as DM

import Data.Char
import Data.List.Split
import Data.Text

-----------------Function Declarations-----------------
requiredDateProvided :: DM.DelimitedRow -> DM.RowHeaderMap -> Bool
requiredDebitCreditXorProvided :: DM.DelimitedRow -> DM.RowHeaderMap -> Bool

----------------Function Implementations----------------
requiredDateProvided delimitedRow rowHeaderMap = let
    splitRow = Data.Text.splitOn "," (pack delimitedRow)
    cellRow = Prelude.map (\x -> if Data.Text.all isSpace x then DM.Empty else DM.Filled (unpack x)) splitRow
    dateIndex = DM.rhmDate rowHeaderMap
    dateCell = cellRow !! dateIndex
    in isNotEmpty dateCell

requiredDebitCreditXorProvided delimitedRow rowHeaderMap = let
    splitRow = Data.Text.splitOn "," (pack delimitedRow)
    cellRow = Prelude.map (\x -> if Data.Text.all isSpace x then DM.Empty else DM.Filled (unpack x)) splitRow
    debitIndex = DM.rhmDebit rowHeaderMap
    creditIndex = DM.rhmCredit rowHeaderMap
    creditCell = cellRow !! creditIndex
    debitCell = cellRow !! debitIndex
    in (isNotEmpty debitCell || isNotEmpty creditCell) && (not (isNotEmpty debitCell) || not (isNotEmpty creditCell))

isNotEmpty cell = case cell of DM.Empty -> False
                               DM.Filled _ -> True
