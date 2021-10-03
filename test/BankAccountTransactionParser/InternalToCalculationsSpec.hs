module BankAccountTransactionParser.InternalToCalculationsSpec where

import Test.Hspec

import BankAccountTransactionParser.DomainModels
import BankAccountTransactionParser.InternalToCalculations

import BankAccountTransactionParser.CellRowParser.DomainModels

import Data.Time
import Data.Time.Format

spec :: Spec
spec = do 
    describe "createBankAccountTransactionFromCellRow" $ do
        it "should return correct bank account transaction when inputting valid cell row and compatible row header map case 1" $ do
            let inputCellRow = [ Filled "Cleared"
                               , Filled "08/31/2021"
                               , Filled "AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA"
                               , Filled "28.18" 
                               , Empty ]
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 4 }
            let expectedResult = BankAccountTransaction { 
                    date = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "08/31/2021" :: UTCTime,
                    description = "AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA",
                    debit = 28.18,
                    credit =  0
                }

            createBankAccountTransactionFromCellRow inputCellRow inputRowHeaderMap `shouldBe` expectedResult

        it "should return correct bank account transaction when inputting valid cell row and compatible row header map case 2" $ do
            let inputCellRow = [ Filled "Cleared"
                               , Filled "08/31/2021"
                               , Filled "AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA"
                               , Filled "5.00" 
                               , Empty ]
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 4 }
            let expectedResult = BankAccountTransaction { 
                    date = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "08/31/2021" :: UTCTime,
                    description = "AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA",
                    debit = 5.00,
                    credit =  0
                }

            createBankAccountTransactionFromCellRow inputCellRow inputRowHeaderMap `shouldBe` expectedResult
