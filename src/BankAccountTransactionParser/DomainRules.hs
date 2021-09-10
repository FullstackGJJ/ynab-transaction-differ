module BankAccountTransactionParser.DomainRules where

import qualified BankAccountTransactionParser.DomainModels as DM

-----------------Function Declarations-----------------
dateProvided :: DM.DelimitedRow -> DM.RowHeaderMap -> Bool
debitCreditXorProvided :: DM.DelimitedRow -> DM.RowHeaderMap -> Bool

----------------Function Implementations----------------
dateProvided delimitedRow rowHeaderMap = False

-- (debit exists or credit exists) and (debit doesn't exist or credit doesn't exist)
debitCreditXorProvided delimitedRow rowHeaderMap = False

