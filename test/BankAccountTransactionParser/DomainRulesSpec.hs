module BankAccountTransactionParser.DomainRulesSpec where

import Test.Hspec

import BankAccountTransactionParser.DomainModels
import BankAccountTransactionParser.CellRowParser.DomainModels
import BankAccountTransactionParser.DomainRules

spec :: Spec
spec = do 
    describe "requiredDateProvided" $ do
        it "should take raw delimited row string, row header map and correctly determine that required date is provided" $ do
            let inputCellRow = [ Filled "Cleared"
                               , Filled "08/31/2021"
                               , Filled "AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA"
                               , Filled "28.18" 
                               , Empty ]
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 4 }
            let expectedResult = True

            requiredDateProvided inputCellRow inputRowHeaderMap `shouldBe` expectedResult

        it "should take raw delimited row string that's missing date, row header map and correctly determine that required date is not provided" $ do
            let inputCellRow = [ Filled "Cleared"
                               , Empty
                               , Filled "AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA"
                               , Filled "28.18" 
                               , Empty ]
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 4 }
            let expectedResult = False

            requiredDateProvided inputCellRow inputRowHeaderMap `shouldBe` expectedResult

    describe "requiredDebitCreditXorProvided" $ do
        it "should take raw delimited row string that doesn't contain debit or credit, take in row header map and correctly determine that it is invalid" $ do
            let inputCellRow = [ Filled "Cleared"
                               , Filled "08/31/2021"
                               , Filled "AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA"
                               , Empty
                               , Empty ]
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 4 }
            let expectedResult = False

            requiredDebitCreditXorProvided inputCellRow inputRowHeaderMap `shouldBe` expectedResult

        it "should take raw delimited row string that doesn't only contains debit and no credit, take in row header map and correctly determine that it is valid" $ do
            let inputCellRow = [ Filled "Cleared"
                               , Filled "08/31/2021"
                               , Filled "AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA"
                               , Filled "1.20"
                               , Empty ]
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 4 }
            let expectedResult = True

            requiredDebitCreditXorProvided inputCellRow inputRowHeaderMap `shouldBe` expectedResult
            
        it "should take raw delimited row string that doesn't only contains credit and no debit, take in row header map and correctly determine that it is valid" $ do
            let inputCellRow = [ Filled "Cleared"
                               , Filled "08/31/2021"
                               , Filled "AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA"
                               , Empty
                               , Filled "5.20" ]
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 4 }
            let expectedResult = True

            requiredDebitCreditXorProvided inputCellRow inputRowHeaderMap `shouldBe` expectedResult

    describe "requiredDebitCreditXorProvided" $ do
        it "should take cell row with row header map that doesn't go out of bounds and determine that it is valid" $ do
            let inputCellRow = [ Filled "Cleared"
                               , Filled "08/31/2021"
                               , Filled "AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA"
                               , Empty
                               , Filled "5.20" ]
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 4 }
            let expectedResult = True

            validConfigurationWithData inputCellRow inputRowHeaderMap `shouldBe` expectedResult

        it "should take cell row with row header map that does go out of bounds and determine that it is invalid" $ do
            let inputCellRow = [ Filled "Cleared"
                               , Filled "08/31/2021"
                               , Filled "AMZN MKTP US*259P09BT0 AMZN.COM/BILL WA"
                               , Empty
                               , Filled "5.20" ]
            let inputRowHeaderMap = RowHeaderMap { rhmDate = 1, rhmDescription = 2, rhmDebit = 3, rhmCredit = 5 }
            let expectedResult = False

            validConfigurationWithData inputCellRow inputRowHeaderMap `shouldBe` expectedResult
