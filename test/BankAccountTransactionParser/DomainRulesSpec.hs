module BankAccountTransactionParser.DomainRulesSpec where

import Test.Hspec

import BankAccountTransactionParser.DomainModels
import BankAccountTransactionParser.DomainRules

spec :: Spec
spec = do 
    describe "requiredDateProvided" $ do
        it "should take raw delimited row string, row header map and correctly determine that required date is provided" $ do
            let inputDelimitedRow = "Cleared,08/31/2021,\"AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA\",28.18,"
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 4 }
            let expectedResult = True

            requiredDateProvided inputDelimitedRow inputRowHeaderMap `shouldBe` expectedResult

        it "should take raw delimited row string that's missing date, row header map and correctly determine that required date is not provided" $ do
            let inputDelimitedRow = "Cleared,,\"AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA\",28.18,"
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 4 }
            let expectedResult = False

            requiredDateProvided inputDelimitedRow inputRowHeaderMap `shouldBe` expectedResult

    describe "requiredDebitCreditXorProvided" $ do
        it "should take raw delimited row string that doesn't contain debit or credit, take in row header map and correctly determine that it is invalid" $ do
            let inputDelimitedRow = "Cleared,08/31/2021,\"AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA\",,"
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 4 }
            let expectedResult = False

            requiredDebitCreditXorProvided inputDelimitedRow inputRowHeaderMap `shouldBe` expectedResult

        it "should take raw delimited row string that doesn't only contains debit and no credit, take in row header map and correctly determine that it is valid" $ do
            let inputDelimitedRow = "Cleared,08/31/2021,\"AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA\",1.20,"
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 4 }
            let expectedResult = True

            requiredDebitCreditXorProvided inputDelimitedRow inputRowHeaderMap `shouldBe` expectedResult
            
        it "should take raw delimited row string that doesn't only contains credit and no debit, take in row header map and correctly determine that it is valid" $ do
            let inputDelimitedRow = "Cleared,08/31/2021,\"AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA\",,5.20"
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 4 }
            let expectedResult = True

            requiredDebitCreditXorProvided inputDelimitedRow inputRowHeaderMap `shouldBe` expectedResult
