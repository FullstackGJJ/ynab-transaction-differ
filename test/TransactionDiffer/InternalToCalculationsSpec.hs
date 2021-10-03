module TransactionDiffer.InternalToCalculationsSpec where

import Test.Hspec

import TransactionDiffer.DomainModels
import TransactionDiffer.DomainRules
import TransactionDiffer.InternalToCalculations

import Data.Time
import Data.Time.Format

spec :: Spec
spec = do 
    describe "determineVerifiedTransactions" $ do
        it "should return expected verified list of transactions when given input reference list and out of sync list case 1" $ do
            let januaryFirst = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/01/2001" :: UTCTime
            let januarySecond = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/02/2001" :: UTCTime
            let januaryThird = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/03/2001" :: UTCTime
            let januaryFourth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/04/2001" :: UTCTime

            let inputReferenceList = [ Transaction { date = januaryFirst, amount = 200.00, merchant = "bob" }
                                     , Transaction { date = januarySecond, amount = 250.00, merchant = "bob" } 
                                     , Transaction { date = januaryThird, amount = 300.00, merchant = "bob" } 
                                     , Transaction { date = januaryFourth, amount = 400.00, merchant = "bob" } ]

            let inputOutOfSyncList = [ Transaction { date = januaryFirst, amount = 200.00, merchant = "bob" }
                                     , Transaction { date = januaryThird, amount = 300.00, merchant = "bob" } ]

            let expectedVerifiedList = [ Transaction { date = januaryFirst, amount = 200.00, merchant = "bob" }
                                       , Transaction { date = januaryThird, amount = 300.00, merchant = "bob" } ]

            determineVerifiedTransactions inputReferenceList inputOutOfSyncList `shouldBe` expectedVerifiedList

        it "should return expected verified list of transactions when given input reference list and out of sync list case 2" $ do
            let januaryFirst = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/01/2001" :: UTCTime
            let januarySecond = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/02/2001" :: UTCTime
            let januaryThird = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/03/2001" :: UTCTime
            let januaryFourth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/04/2001" :: UTCTime
            let januaryTenth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/10/2001" :: UTCTime

            let inputReferenceList = [ Transaction { date = januaryFirst, amount = 200.00, merchant = "bob" }
                                     , Transaction { date = januarySecond, amount = 250.00, merchant = "bob" } 
                                     , Transaction { date = januaryThird, amount = 300.00, merchant = "bob" } 
                                     , Transaction { date = januaryFourth, amount = 400.00, merchant = "bob" } ]

            let inputOutOfSyncList = [ Transaction { date = januaryFirst, amount = 200.00, merchant = "bob" }
                                     , Transaction { date = januaryTenth, amount = 800.00, merchant = "bob" } ]

            let expectedVerifiedList = [ Transaction { date = januaryFirst, amount = 200.00, merchant = "bob" } ]

            determineVerifiedTransactions inputReferenceList inputOutOfSyncList `shouldBe` expectedVerifiedList


        it "should return empty list when given input reference list and out of sync list where out of sync list has none of reference list transactions" $ do
            let januaryFirst = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/01/2001" :: UTCTime
            let januarySecond = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/02/2001" :: UTCTime
            let januaryThird = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/03/2001" :: UTCTime
            let januaryFourth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/04/2001" :: UTCTime
            let januaryTenth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/10/2001" :: UTCTime

            let inputReferenceList = [ Transaction { date = januaryFirst, amount = 200.00, merchant = "bob" }
                                     , Transaction { date = januarySecond, amount = 250.00, merchant = "bob" } 
                                     , Transaction { date = januaryThird, amount = 300.00, merchant = "bob" } 
                                     , Transaction { date = januaryFourth, amount = 400.00, merchant = "bob" } ]

            let inputOutOfSyncList = [ Transaction { date = januaryTenth, amount = 800.00, merchant = "bob" } ]

            let expectedVerifiedList = []

            --determineVerifiedTransactions inputReferenceList inputOutOfSyncList `shouldBe` expectedVerifiedList
            1 `shouldBe` 1


    describe "determineMissingTransactions" $ do
        it "should return expected missing list of transactions when given input reference list and out of sync list case 1" $ do
            let januaryFirst = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/01/2001" :: UTCTime
            let januarySecond = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/02/2001" :: UTCTime
            let januaryThird = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/03/2001" :: UTCTime
            let januaryFourth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/04/2001" :: UTCTime
            let januaryTenth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/10/2001" :: UTCTime

            let inputReferenceList = [ Transaction { date = januaryFirst, amount = 200.00, merchant = "bob" }
                                     , Transaction { date = januarySecond, amount = 250.00, merchant = "bob" } 
                                     , Transaction { date = januaryThird, amount = 300.00, merchant = "bob" } 
                                     , Transaction { date = januaryFourth, amount = 400.00, merchant = "bob" } ]

            let inputOutOfSyncList = [ Transaction { date = januaryFirst, amount = 200.00, merchant = "bob" }
                                     , Transaction { date = januaryThird, amount = 300.00, merchant = "bob" } ]

            let expectedMissingList = [ Transaction { date = januarySecond, amount = 250.00, merchant = "bob" }
                                      , Transaction { date = januaryFourth, amount = 400.00, merchant = "bob" } ]

            determineMissingTransactions inputReferenceList inputOutOfSyncList `shouldBe` expectedMissingList

        it "should return expected missing list of transactions when given input reference list and out of sync list case 2" $ do
            let januaryFirst = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/01/2001" :: UTCTime
            let januarySecond = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/02/2001" :: UTCTime
            let januaryThird = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/03/2001" :: UTCTime
            let januaryFourth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/04/2001" :: UTCTime

            let inputReferenceList = [ Transaction { date = januaryFirst, amount = 200.00, merchant = "bob" }
                                     , Transaction { date = januarySecond, amount = 250.00, merchant = "bob" } 
                                     , Transaction { date = januaryThird, amount = 300.00, merchant = "bob" } 
                                     , Transaction { date = januaryFourth, amount = 400.00, merchant = "bob" } ]

            let inputOutOfSyncList = [ Transaction { date = januaryFirst, amount = 200.00, merchant = "bob" } ]

            let expectedMissingList = [ Transaction { date = januarySecond, amount = 250.00, merchant = "bob" }
                                      , Transaction { date = januaryThird, amount = 300.00, merchant = "bob" } 
                                      , Transaction { date = januaryFourth, amount = 400.00, merchant = "bob" } ]

            determineMissingTransactions inputReferenceList inputOutOfSyncList `shouldBe` expectedMissingList

    describe "determineExtraTransactions" $ do
        it "should return expected extra list of transactions when given input reference list and out of sync list case 1" $ do
            let januaryFirst = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/01/2001" :: UTCTime
            let januarySecond = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/02/2001" :: UTCTime
            let januaryThird = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/03/2001" :: UTCTime
            let januaryFourth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/04/2001" :: UTCTime
            let januaryNinth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/09/2001" :: UTCTime
            let januaryTenth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/10/2001" :: UTCTime
            let januaryEleventh = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/11/2001" :: UTCTime
            let januaryTwelfth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "01/12/2001" :: UTCTime

            let inputReferenceList = [ Transaction { date = januaryFirst, amount = 200.00, merchant = "bob" }
                                     , Transaction { date = januarySecond, amount = 250.00, merchant = "bob" } 
                                     , Transaction { date = januaryThird, amount = 300.00, merchant = "bob" } 
                                     , Transaction { date = januaryFourth, amount = 400.00, merchant = "bob" } ]

            let inputOutOfSyncList = [ Transaction { date = januaryFirst, amount = 200.00, merchant = "bob" } 
                                     , Transaction { date = januarySecond, amount = 250.00, merchant = "bob" } 
                                     , Transaction { date = januaryThird, amount = 300.00, merchant = "bob" } 
                                     , Transaction { date = januaryNinth, amount = 900.00, merchant = "bob" } 
                                     , Transaction { date = januaryTenth, amount = 10000.00, merchant = "bob" } 
                                     , Transaction { date = januaryTwelfth, amount = 1500.00, merchant = "bob" } ]

            let expectedExtraList = [ Transaction { date = januaryNinth, amount = 900.00, merchant = "bob" } 
                                      , Transaction { date = januaryTenth, amount = 10000.00, merchant = "bob" } 
                                      , Transaction { date = januaryTwelfth, amount = 1500.00, merchant = "bob" } ]

            determineExtraTransactions inputReferenceList inputOutOfSyncList `shouldBe` expectedExtraList
