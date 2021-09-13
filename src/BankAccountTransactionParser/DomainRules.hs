{-# LANGUAGE OverloadedStrings #-}
module BankAccountTransactionParser.DomainRules where

import qualified BankAccountTransactionParser.DomainModels as DM
import qualified BankAccountTransactionParser.CellRowParser.DomainModels as CRP_DM
import qualified BankAccountTransactionParser.CellRowParser.PureCalculations as CRP_PC

import Data.Char
import Data.List.Split
import Data.Text

-----------------Function Declarations-----------------
validConfigurationWithData :: CRP_DM.CellRow -> DM.RowHeaderMap -> Bool
requiredDateProvided :: CRP_DM.CellRow -> DM.RowHeaderMap -> Bool
requiredDebitCreditXorProvided :: CRP_DM.CellRow -> DM.RowHeaderMap -> Bool

----------------Function Implementations----------------
validConfigurationWithData cellRow rowHeaderMap = let
    indices = [ DM.rhmDate rowHeaderMap
              , DM.rhmDescription rowHeaderMap
              , DM.rhmDebit rowHeaderMap
              , DM.rhmCredit rowHeaderMap ]
    cellRowLength = Prelude.length cellRow
    containsOutOfBoundsIndex = Prelude.any (>= cellRowLength) indices
    in not containsOutOfBoundsIndex

requiredDateProvided cellRow rowHeaderMap = let
    dateIndex = DM.rhmDate rowHeaderMap
    dateCell = cellRow !! dateIndex
    in CRP_PC.isNotEmpty dateCell

requiredDebitCreditXorProvided cellRow rowHeaderMap = let
    debitIndex = DM.rhmDebit rowHeaderMap
    creditIndex = DM.rhmCredit rowHeaderMap
    creditCell = cellRow !! creditIndex
    debitCell = cellRow !! debitIndex
    in (CRP_PC.isNotEmpty debitCell || CRP_PC.isNotEmpty creditCell) && (not (CRP_PC.isNotEmpty debitCell) || not (CRP_PC.isNotEmpty creditCell))

