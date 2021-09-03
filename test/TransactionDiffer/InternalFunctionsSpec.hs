module TransactionDiffer.InternalFunctionsSpec where

import Test.Hspec

import TransactionDiffer.DomainModels
import TransactionDiffer.DomainRules
import TransactionDiffer.InternalFunctions

spec :: Spec
spec = do 
    describe "determineVerifiedTransactions" $ do
        it "should return expected verified list of transactions when given input reference list and out of sync list case 1" $ do
            let inputReferenceList = [ Transaction { date = "01/01/2001", amount = 200.00, merchant = "bob" }
                                     , Transaction { date = "01/02/2001", amount = 250.00, merchant = "bob" } 
                                     , Transaction { date = "01/03/2001", amount = 300.00, merchant = "bob" } 
                                     , Transaction { date = "01/04/2001", amount = 400.00, merchant = "bob" } ]

            let inputOutOfSyncList = [ Transaction { date = "01/01/2001", amount = 200.00, merchant = "bob" }
                                     , Transaction { date = "01/03/2001", amount = 300.00, merchant = "bob" } ]

            let expectedVerifiedList = [ Transaction { date = "01/01/2001", amount = 200.00, merchant = "bob" }
                                       , Transaction { date = "01/03/2001", amount = 300.00, merchant = "bob" } ]

            determineVerifiedTransactions inputReferenceList inputOutOfSyncList `shouldBe` expectedVerifiedList

        it "should return expected verified list of transactions when given input reference list and out of sync list case 2" $ do
            let inputReferenceList = [ Transaction { date = "01/01/2001", amount = 200.00, merchant = "bob" }
                                     , Transaction { date = "01/02/2001", amount = 250.00, merchant = "bob" } 
                                     , Transaction { date = "01/03/2001", amount = 300.00, merchant = "bob" } 
                                     , Transaction { date = "01/04/2001", amount = 400.00, merchant = "bob" } ]

            let inputOutOfSyncList = [ Transaction { date = "01/01/2001", amount = 200.00, merchant = "bob" }
                                     , Transaction { date = "01/10/2001", amount = 800.00, merchant = "bob" } ]

            let expectedVerifiedList = [ Transaction { date = "01/01/2001", amount = 200.00, merchant = "bob" } ]

            determineVerifiedTransactions inputReferenceList inputOutOfSyncList `shouldBe` expectedVerifiedList

    describe "determineMissingTransactions" $ do
        it "should return expected missing list of transactions when given input reference list and out of sync list case 1" $ do
            let inputReferenceList = [ Transaction { date = "01/01/2001", amount = 200.00, merchant = "bob" }
                                     , Transaction { date = "01/02/2001", amount = 250.00, merchant = "bob" } 
                                     , Transaction { date = "01/03/2001", amount = 300.00, merchant = "bob" } 
                                     , Transaction { date = "01/04/2001", amount = 400.00, merchant = "bob" } ]

            let inputOutOfSyncList = [ Transaction { date = "01/01/2001", amount = 200.00, merchant = "bob" }
                                     , Transaction { date = "01/03/2001", amount = 300.00, merchant = "bob" } ]

            let expectedMissingList = [ Transaction { date = "01/02/2001", amount = 250.00, merchant = "bob" }
                                      , Transaction { date = "01/04/2001", amount = 400.00, merchant = "bob" } ]

            determineMissingTransactions inputReferenceList inputOutOfSyncList `shouldBe` expectedMissingList

        it "should return expected missing list of transactions when given input reference list and out of sync list case 2" $ do
            let inputReferenceList = [ Transaction { date = "01/01/2001", amount = 200.00, merchant = "bob" }
                                     , Transaction { date = "01/02/2001", amount = 250.00, merchant = "bob" } 
                                     , Transaction { date = "01/03/2001", amount = 300.00, merchant = "bob" } 
                                     , Transaction { date = "01/04/2001", amount = 400.00, merchant = "bob" } ]

            let inputOutOfSyncList = [ Transaction { date = "01/01/2001", amount = 200.00, merchant = "bob" } ]

            let expectedMissingList = [ Transaction { date = "01/02/2001", amount = 250.00, merchant = "bob" }
                                      , Transaction { date = "01/03/2001", amount = 300.00, merchant = "bob" } 
                                      , Transaction { date = "01/04/2001", amount = 400.00, merchant = "bob" } ]

            determineMissingTransactions inputReferenceList inputOutOfSyncList `shouldBe` expectedMissingList

    describe "determineExtraTransactions" $ do
        it "should return expected extra list of transactions when given input reference list and out of sync list case 1" $ do
            let inputReferenceList = [ Transaction { date = "01/01/2001", amount = 200.00, merchant = "bob" }
                                     , Transaction { date = "01/02/2001", amount = 250.00, merchant = "bob" } 
                                     , Transaction { date = "01/03/2001", amount = 300.00, merchant = "bob" } 
                                     , Transaction { date = "01/04/2001", amount = 400.00, merchant = "bob" } ]

            let inputOutOfSyncList = [ Transaction { date = "01/01/2001", amount = 200.00, merchant = "bob" } 
                                     , Transaction { date = "01/02/2001", amount = 250.00, merchant = "bob" } 
                                     , Transaction { date = "01/03/2001", amount = 300.00, merchant = "bob" } 
                                     , Transaction { date = "01/09/2001", amount = 900.00, merchant = "bob" } 
                                     , Transaction { date = "01/10/2001", amount = 10000.00, merchant = "bob" } 
                                     , Transaction { date = "01/12/2001", amount = 1500.00, merchant = "bob" } ]

            let expectedMissingList = [ Transaction { date = "01/09/2001", amount = 900.00, merchant = "bob" } 
                                      , Transaction { date = "01/10/2001", amount = 10000.00, merchant = "bob" } 
                                      , Transaction { date = "01/12/2001", amount = 1500.00, merchant = "bob" } ]

            determineExtraTransactions inputReferenceList inputOutOfSyncList `shouldBe` expectedMissingList