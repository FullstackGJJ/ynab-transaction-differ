module DomainRulesSpec where

import Test.Hspec

import DomainModels
import DomainRules

import Data.Map

spec :: Spec
spec = do 
    describe "containsMatchingDates" $ do
        it "should return false when given two date mapped transactions with mismatching dates" $ do
            let inputDateMappedTransactions1 = fromList [("08/31/2020", [])]
            let inputDateMappedTransactions2 = fromList []
            let expectedResult = False

            containsMatchingDates inputDateMappedTransactions1 inputDateMappedTransactions2 `shouldBe` expectedResult

        it "should return true when given two date mapped transactions with matching dates" $ do
            let inputDateMappedTransactions1 = fromList [("08/31/2020", [])]
            let inputDateMappedTransactions2 = fromList [("08/31/2020", [])]
            let expectedResult = True

            containsMatchingDates inputDateMappedTransactions1 inputDateMappedTransactions2 `shouldBe` expectedResult

    describe "populateMissingDates" $ do
        it "should return an updated date mapped transaction collection that contains all of the dates it had originally and also dates from the reference case 1" $ do
            let inputReferenceCollection = fromList [("08/31/2020", [])]
            let inputTargetCollection = fromList []
            let expectedResult = fromList [("08/31/2020", [])]

            populateMissingDates inputReferenceCollection inputTargetCollection `shouldBe` expectedResult

        it "should return an updated date mapped transaction collection that contains all of the dates it had originally and also dates from the reference case 2" $ do
            let inputReferenceCollection = fromList [("08/31/2020", [])]
            let inputTargetCollection = fromList [("08/21/2021", [])]
            let expectedResult = fromList [ ("08/31/2020", [])
                                          , ("08/21/2021", [])
                                          ]

            populateMissingDates inputReferenceCollection inputTargetCollection `shouldBe` expectedResult
