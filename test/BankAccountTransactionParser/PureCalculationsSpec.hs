module BankAccountTransactionParser.PureCalculationsSpec where

import Test.Hspec

import BankAccountTransactionParser.DomainModels
import BankAccountTransactionParser.DomainRules
import BankAccountTransactionParser.PureCalculations

import BankAccountTransactionParser.CellRowParser.DomainModels

spec :: Spec
spec = do 
    describe "createBankAccountTransaction" $ do
        it "should return correct bank account transaction when inputting valid delimited row and compatible row header map case 1" $ do
            let inputDelimitedRow = "Cleared,08/31/2021,AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA,28.18,"
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 4 }
            let expectedResult = Right BankAccountTransaction { 
                    date = "08/31/2021",
                    description = "AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA",
                    debit = 28.18,
                    credit =  0
                }

            createBankAccountTransaction inputDelimitedRow inputRowHeaderMap `shouldBe` expectedResult

        it "should return correct bank account transaction when inputting valid delimited row and compatible row header map case 2" $ do
            let inputDelimitedRow = "Cleared,08/30/2021,AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA,5.00,"
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 4 }
            let expectedResult = Right BankAccountTransaction { 
                    date = "08/30/2021",
                    description = "AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA",
                    debit = 5.00,
                    credit =  0
                }

            createBankAccountTransaction inputDelimitedRow inputRowHeaderMap `shouldBe` expectedResult
