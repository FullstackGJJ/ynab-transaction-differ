module TransactionDiffer.PureCalculationsSpec where

import Test.Hspec

import TransactionDiffer.DomainModels
import TransactionDiffer.PureCalculations

spec :: Spec
spec = do 
    describe "findTransactionDiffs" $ do
        it "should take 2 transaction lists, one referenceList, one outOfSyncList, and accurately give back verifiedList, missingList, and extraList case 1" $ do
            let inputReferenceList = [ Transaction { date = "01/01/2001", amount = 1.50, merchant = "bob" }
                                     , Transaction { date = "01/02/2001", amount = 2.50, merchant = "jackie" } ]
            let inputOutOfSyncList = [ Transaction { date = "01/01/2001", amount = 1.50, merchant = "bob" } 
                                     , Transaction { date = "01/05/2001", amount = 4.50, merchant = "sage" } ]
            let expectedVerifiedList = [ Transaction { date = "01/01/2001", amount = 1.50, merchant = "bob" } ]
            let expectedMissingList = [ Transaction { date = "01/02/2001", amount = 2.50, merchant = "jackie" } ]
            let expectedExtraList = [ Transaction { date = "01/05/2001", amount = 4.50, merchant = "sage" } ]

            findTransactionDiffs inputReferenceList inputOutOfSyncList `shouldBe` (expectedVerifiedList, expectedMissingList, expectedExtraList)
