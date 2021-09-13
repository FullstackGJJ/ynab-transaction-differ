module BankAccountTransactionParser.InternalToCalculations where

import qualified BankAccountTransactionParser.DomainModels as DM

import qualified BankAccountTransactionParser.CellRowParser.DomainModels as CRP_DM

-----------------Function Declarations-----------------
createBankAccountTransactionFromCellRow :: CRP_DM.CellRow -> DM.RowHeaderMap -> DM.BankAccountTransaction

----------------Function Implementations----------------
createBankAccountTransactionFromCellRow cellRow rowHeaderMap = let
    date = case (cellRow !! (DM.rhmDate rowHeaderMap)) of CRP_DM.Empty -> ""; CRP_DM.Filled x -> x
    description = case (cellRow !! (DM.rhmDescription rowHeaderMap)) of CRP_DM.Empty -> ""; CRP_DM.Filled x -> x
    debitString = case (cellRow !! (DM.rhmDebit rowHeaderMap)) of CRP_DM.Empty -> "0"; CRP_DM.Filled x -> x
    creditString = case (cellRow !! (DM.rhmCredit rowHeaderMap)) of CRP_DM.Empty -> "0"; CRP_DM.Filled x -> x
    in DM.BankAccountTransaction {
        DM.date = date,
        DM.description = description,
        DM.debit = read (debitString) :: Float,
        DM.credit = read (creditString) :: Float
    }
