module DomainRulesSpec where

import Test.Hspec

import DomainModels
import DomainRules

import Data.Map

import Data.Time
import Data.Time.Format

spec :: Spec
spec = do 
    describe "containsMatchingDates" $ do
        it "should return false when given two date mapped transactions with mismatching dates" $ do
            let date = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "08/31/2021" :: UTCTime
            let inputDateMappedTransactions1 = fromList [(date, [])]
            let inputDateMappedTransactions2 = fromList []
            let expectedResult = False

            containsMatchingDates inputDateMappedTransactions1 inputDateMappedTransactions2 `shouldBe` expectedResult

        it "should return true when given two date mapped transactions with matching dates" $ do
            let date = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "08/31/2021" :: UTCTime
            let inputDateMappedTransactions1 = fromList [(date, [])]
            let inputDateMappedTransactions2 = fromList [(date, [])]
            let expectedResult = True

            containsMatchingDates inputDateMappedTransactions1 inputDateMappedTransactions2 `shouldBe` expectedResult

    describe "populateMissingDates" $ do
        it "should return an updated date mapped transaction collection that contains all of the dates it had originally and also dates from the reference case 1" $ do
            let date = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "08/31/2021" :: UTCTime
            let inputReferenceCollection = fromList [(date, [])]
            let inputTargetCollection = fromList []
            let expectedResult = fromList [(date, [])]

            populateMissingDates inputReferenceCollection inputTargetCollection `shouldBe` expectedResult

        it "should return an updated date mapped transaction collection that contains all of the dates it had originally and also dates from the reference case 2" $ do
            let augustThirtyFirst = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "08/31/2021" :: UTCTime
            let augustTwentyFirst = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "08/21/2021" :: UTCTime
            let inputReferenceCollection = fromList [(augustThirtyFirst, [])]
            let inputTargetCollection = fromList [(augustTwentyFirst, [])]
            let expectedResult = fromList [ (augustThirtyFirst, [])
                                          , (augustTwentyFirst, [])
                                          ]

            populateMissingDates inputReferenceCollection inputTargetCollection `shouldBe` expectedResult
