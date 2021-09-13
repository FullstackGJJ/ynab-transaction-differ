module BankAccountTransactionParser.PureCalculations where

import qualified BankAccountTransactionParser.DomainModels as DM
import qualified BankAccountTransactionParser.DomainRules as DR
import qualified BankAccountTransactionParser.InternalToCalculations as I

import qualified BankAccountTransactionParser.CellRowParser.DomainModels as CRP_DM
import qualified BankAccountTransactionParser.CellRowParser.PureCalculations as CRP_PC

import Data.Text

-----------------Function Declarations-----------------
createBankAccountTransaction :: CRP_DM.DelimitedRow -> DM.RowHeaderMap -> Either DM.RowParsingError DM.BankAccountTransaction

----------------Function Implementations----------------
createBankAccountTransaction delimitedRow rowHeaderMap = let
    cellRow = CRP_PC.convertDelimitedToCellRow (pack ",") delimitedRow
    isValidInput = DR.validConfigurationWithData cellRow rowHeaderMap &&
        DR.requiredDateProvided cellRow rowHeaderMap &&
        DR.requiredDebitCreditXorProvided cellRow rowHeaderMap
    in if isValidInput
       then Right (I.createBankAccountTransactionFromCellRow cellRow rowHeaderMap)
       else Left DM.RowParsingError { DM.reason = "Error in parsing input" }
