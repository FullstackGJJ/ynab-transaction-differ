module TransactionDiffer.PureCalculationsSpec where

import Test.Hspec

import TransactionDiffer.DomainModels
import TransactionDiffer.PureCalculations

import Data.Time
import Data.Time.Format

januaryFirst = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/01/2001" :: UTCTime
januarySecond = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/02/2001" :: UTCTime
januaryThird = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/03/2001" :: UTCTime
januaryFourth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/04/2001" :: UTCTime
januaryFifth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/05/2001" :: UTCTime
januaryNinth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/09/2001" :: UTCTime
januaryTenth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/10/2001" :: UTCTime
januaryEleventh = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/11/2001" :: UTCTime
januaryTwelfth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/12/2001" :: UTCTime

spec :: Spec
spec = do 
    describe "findTransactionDiffs" $ do
        it "should take 2 transaction lists, one referenceList, one outOfSyncList, and accurately give back expected transactions diff case 1" $ do
            let inputReferenceList = [ Transaction { date = januaryFirst, amount = 1.50, merchant = "bob" }
                                     , Transaction { date = januarySecond, amount = 2.50, merchant = "jackie" } ]
            let inputOutOfSyncList = [ Transaction { date = januaryFirst, amount = 1.50, merchant = "bob" } 
                                     , Transaction { date = januaryFifth, amount = 4.50, merchant = "sage" } ]
            let expectedVerifiedList = [ Transaction { date = januaryFirst, amount = 1.50, merchant = "bob" } ]
            let expectedMissingList = [ Transaction { date = januarySecond, amount = 2.50, merchant = "jackie" } ]
            let expectedExtraList = [ Transaction { date = januaryFifth, amount = 4.50, merchant = "sage" } ]
            let expectedResult = TransactionsDiff { tdVerified = expectedVerifiedList
                                                  , tdExtra = expectedExtraList
                                                  , tdMissing = expectedMissingList
                                                  }

            findTransactionDiffs inputReferenceList inputOutOfSyncList `shouldBe` expectedResult

        it "should take 2 transaction lists, one referenceList, one outOfSyncList, and accurately give back expected transactions diff case 2" $ do
            let inputReferenceList = [ Transaction { date = januaryFirst, amount = 1.50, merchant = "bob" }
                                     , Transaction { date = januarySecond, amount = 2.50, merchant = "jackie" } ]
            let inputOutOfSyncList = []
            let expectedVerifiedList = []
            let expectedMissingList = [ Transaction { date = januaryFirst, amount = 1.50, merchant = "bob" } 
                                      , Transaction { date = januarySecond, amount = 2.50, merchant = "jackie" } 
                                      ]
            let expectedExtraList = []
            let expectedResult = TransactionsDiff { tdVerified = expectedVerifiedList
                                                  , tdExtra = expectedExtraList
                                                  , tdMissing = expectedMissingList
                                                  }

            findTransactionDiffs inputReferenceList inputOutOfSyncList `shouldBe` expectedResult
