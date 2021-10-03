module PureCalculationsSpec where

import Test.Hspec

import DomainModels
import PureCalculations
import TransactionDiffer.DomainModels

import Data.Map

import Data.Time
import Data.Time.Format

spec :: Spec
spec = do 
    describe "calculateTransactionDiffs" $ do
        it "should return correct date mapped transaction diffs when given two different date mapped transactions case 1" $ do
            let augustThirtyFirst = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "08/31/2020" :: UTCTime
            let inputReferenceTransactions = fromList [(augustThirtyFirst, [ Transaction { date = augustThirtyFirst, amount = 20.01, merchant = "" } ])]
            let inputOutOfSyncTransactions = fromList [(augustThirtyFirst, [ Transaction { date = augustThirtyFirst, amount = 100.00, merchant = "" } ])]
            let expectedResult = fromList [ (augustThirtyFirst, TransactionsDiff { tdVerified = []
                                                                                 , tdExtra = [ Transaction { date = augustThirtyFirst, amount = 100.00, merchant = "" } ]
                                                                                 , tdMissing = [ Transaction { date = augustThirtyFirst, amount = 20.01, merchant = "" } ]
                                                                                 })
                                          ]
              
            calculateTransactionDiffs inputReferenceTransactions inputOutOfSyncTransactions `shouldBe` expectedResult

        it "should return correct date mapped transaction diffs when given two different date mapped transactions case 2" $ do
            let augustThirtyFirst = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "08/31/2020" :: UTCTime
            let inputReferenceTransactions = fromList [(augustThirtyFirst, [ Transaction { date = augustThirtyFirst, amount = 20.01, merchant = "" }
                                                                           , Transaction { date = augustThirtyFirst, amount = 54.02, merchant = "" }
                                                                           , Transaction { date = augustThirtyFirst, amount = 30.00, merchant = "" }
                                                                           , Transaction { date = augustThirtyFirst, amount = 200.00, merchant = "" }
                                                                           ])]
            let inputOutOfSyncTransactions = fromList [(augustThirtyFirst, [ Transaction { date = augustThirtyFirst, amount = 100.00, merchant = "" }
                                                                           , Transaction { date = augustThirtyFirst, amount = 75.00, merchant = "" }
                                                                           ])]
            let expectedResult = fromList [ (augustThirtyFirst, TransactionsDiff { tdVerified = []
                                                                                 , tdExtra = [ Transaction { date = augustThirtyFirst, amount = 100.00, merchant = "" }
                                                                                             , Transaction { date = augustThirtyFirst, amount = 75.00, merchant = "" }
                                                                                             ]
                                                                                 , tdMissing = [ Transaction { date = augustThirtyFirst, amount = 20.01, merchant = "" }
                                                                                               , Transaction { date = augustThirtyFirst, amount = 54.02, merchant = "" }
                                                                                               , Transaction { date = augustThirtyFirst, amount = 30.00, merchant = "" }
                                                                                               , Transaction { date = augustThirtyFirst, amount = 200.00, merchant = "" }
                                                                                               ]
                                                                                 })
                                          ]
              
            calculateTransactionDiffs inputReferenceTransactions inputOutOfSyncTransactions `shouldBe` expectedResult

        it "should return correct date mapped transaction diffs when given two different date mapped transactions case 3" $ do
            let augustThirtyFirst = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "08/31/2020" :: UTCTime
            let augustThirtieth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "08/30/2020" :: UTCTime
            let inputReferenceTransactions = fromList [(augustThirtyFirst, [ Transaction { date = augustThirtyFirst, amount = 20.01, merchant = "" }
                                                                      , Transaction { date = augustThirtyFirst, amount = 54.02, merchant = "" }
                                                                      , Transaction { date = augustThirtyFirst, amount = 30.00, merchant = "" }
                                                                      , Transaction { date = augustThirtyFirst, amount = 200.00, merchant = "" }
                                                                      ])]
            let inputOutOfSyncTransactions = fromList [(augustThirtieth, [ Transaction { date = augustThirtieth, amount = 100.00, merchant = "" }
                                                                      , Transaction { date = augustThirtieth, amount = 75.00, merchant = "" }
                                                                      ])]
            let expectedResult = fromList [ (augustThirtyFirst, TransactionsDiff { tdVerified = []
                                                                            , tdExtra = []
                                                                            , tdMissing = [ Transaction { date = augustThirtyFirst, amount = 20.01, merchant = "" }
                                                                                          , Transaction { date = augustThirtyFirst, amount = 54.02, merchant = "" }
                                                                                          , Transaction { date = augustThirtyFirst, amount = 30.00, merchant = "" }
                                                                                          , Transaction { date = augustThirtyFirst, amount = 200.00, merchant = "" }
                                                                                          ]
                                                                            })
                                          , (augustThirtieth, TransactionsDiff { tdVerified = []
                                                                            , tdMissing = []
                                                                            , tdExtra = [ Transaction { date = augustThirtieth, amount = 100.00, merchant = "" }
                                                                                        , Transaction { date = augustThirtieth, amount = 75.00, merchant = "" }
                                                                                        ]
                                                                            })
                                          ]
              
            calculateTransactionDiffs inputReferenceTransactions inputOutOfSyncTransactions `shouldBe` expectedResult

        it "should return correct date mapped transaction diffs when given two different date mapped transactions case 4" $ do
            let augustThirtyFirst = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "08/31/2020" :: UTCTime
            let augustThirtieth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "08/30/2020" :: UTCTime
            let augustTwentieth = parseTimeOrError False defaultTimeLocale "%m/%d/%Y" "08/20/2020" :: UTCTime
            let inputReferenceTransactions = fromList [ (augustThirtyFirst, [ Transaction { date = augustThirtyFirst, amount = 20.01, merchant = "" }
                                                                            , Transaction { date = augustThirtyFirst, amount = 54.02, merchant = "" }
                                                                            , Transaction { date = augustThirtyFirst, amount = 30.00, merchant = "" }
                                                                            , Transaction { date = augustThirtyFirst, amount = 200.00, merchant = "" }
                                                                            ])
                                                      , (augustTwentieth, [ Transaction { date = augustTwentieth, amount = 500.00, merchant = "" } ])
                                                      ]
            let inputOutOfSyncTransactions = fromList [ (augustThirtieth, [ Transaction { date = augustThirtieth, amount = 100.00, merchant = "" }
                                                                            , Transaction { date = augustThirtieth, amount = 75.00, merchant = "" }
                                                                            ])
                                                      , (augustTwentieth, [ Transaction { date = augustTwentieth, amount = 500.00, merchant = "" } ])
                                                      ]
            let expectedResult = fromList [ (augustThirtyFirst, TransactionsDiff { tdVerified = []
                                                                                 , tdExtra = []
                                                                                 , tdMissing = [ Transaction { date = augustThirtyFirst, amount = 20.01, merchant = "" }
                                                                                               , Transaction { date = augustThirtyFirst, amount = 54.02, merchant = "" }
                                                                                               , Transaction { date = augustThirtyFirst, amount = 30.00, merchant = "" }
                                                                                               , Transaction { date = augustThirtyFirst, amount = 200.00, merchant = "" }
                                                                                               ]
                                                                                 })
                                          , (augustThirtieth, TransactionsDiff { tdVerified = []
                                                                                 , tdMissing = []
                                                                                 , tdExtra = [ Transaction { date = augustThirtieth, amount = 100.00, merchant = "" }
                                                                                             , Transaction { date = augustThirtieth, amount = 75.00, merchant = "" }
                                                                                             ]
                                                                                 })
                                          , (augustTwentieth, TransactionsDiff { tdExtra = []
                                                                               , tdMissing = []
                                                                               , tdVerified = [ Transaction { date = augustTwentieth, amount = 500.00, merchant = "" } ]
                                                                               })
                                          ]
              
            calculateTransactionDiffs inputReferenceTransactions inputOutOfSyncTransactions `shouldBe` expectedResult
