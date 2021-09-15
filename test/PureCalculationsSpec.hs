module PureCalculationsSpec where

import Test.Hspec

import DomainModels
import PureCalculations

--spec :: Spec
--spec = do 
--    describe "containsMatchingDates" $ do
--        it "should return false when given two date mapped transactions with mismatching dates" $ do
--            let inputDateMappedTransactions1 = fromList [("08/31/2020", [])]
--            let inputDateMappedTransactions2 = fromList []
--            let expectedResult = False
--
--            containsMatchingDates inputDateMappedTransactions1 inputDateMappedTransactions2 `shouldBe` expectedResult
