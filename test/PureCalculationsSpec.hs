module PureCalculationsSpec where

import Test.Hspec

import DomainModels
import PureCalculations
import TransactionDiffer.DomainModels

import Data.Map

spec :: Spec
spec = do 
    describe "calculateTransactionDiffs" $ do
        it "should return correct date mapped transaction diffs when given two different date mapped transactions case 1" $ do
            let inputReferenceTransactions = fromList [("08/31/2020", [ Transaction { date = "08/31/2020", amount = 20.01, merchant = "" } ])]
            let inputOutOfSyncTransactions = fromList [("08/31/2020", [ Transaction { date = "08/31/2020", amount = 100.00, merchant = "" } ])]
            let expectedResult = fromList [ ("08/31/2020", TransactionsDiff { tdVerified = []
                                                                            , tdExtra = [ Transaction { date = "08/31/2020", amount = 100.00, merchant = "" } ]
                                                                            , tdMissing = [ Transaction { date = "08/31/2020", amount = 20.01, merchant = "" } ]
                                                                            })
                                          ]
              
            calculateTransactionDiffs inputReferenceTransactions inputOutOfSyncTransactions `shouldBe` expectedResult

        it "should return correct date mapped transaction diffs when given two different date mapped transactions case 2" $ do
            let inputReferenceTransactions = fromList [("08/31/2020", [ Transaction { date = "08/31/2020", amount = 20.01, merchant = "" }
                                                                      , Transaction { date = "08/31/2020", amount = 54.02, merchant = "" }
                                                                      , Transaction { date = "08/31/2020", amount = 30.00, merchant = "" }
                                                                      , Transaction { date = "08/31/2020", amount = 200.00, merchant = "" }
                                                                      ])]
            let inputOutOfSyncTransactions = fromList [("08/31/2020", [ Transaction { date = "08/31/2020", amount = 100.00, merchant = "" }
                                                                      , Transaction { date = "08/31/2020", amount = 75.00, merchant = "" }
                                                                      ])]
            let expectedResult = fromList [ ("08/31/2020", TransactionsDiff { tdVerified = []
                                                                            , tdExtra = [ Transaction { date = "08/31/2020", amount = 100.00, merchant = "" }
                                                                                        , Transaction { date = "08/31/2020", amount = 75.00, merchant = "" }
                                                                                        ]
                                                                            , tdMissing = [ Transaction { date = "08/31/2020", amount = 20.01, merchant = "" }
                                                                                          , Transaction { date = "08/31/2020", amount = 54.02, merchant = "" }
                                                                                          , Transaction { date = "08/31/2020", amount = 30.00, merchant = "" }
                                                                                          , Transaction { date = "08/31/2020", amount = 200.00, merchant = "" }
                                                                                          ]
                                                                            })
                                          ]
              
            calculateTransactionDiffs inputReferenceTransactions inputOutOfSyncTransactions `shouldBe` expectedResult

        it "should return correct date mapped transaction diffs when given two different date mapped transactions case 3" $ do
            let inputReferenceTransactions = fromList [("08/31/2020", [ Transaction { date = "08/31/2020", amount = 20.01, merchant = "" }
                                                                      , Transaction { date = "08/31/2020", amount = 54.02, merchant = "" }
                                                                      , Transaction { date = "08/31/2020", amount = 30.00, merchant = "" }
                                                                      , Transaction { date = "08/31/2020", amount = 200.00, merchant = "" }
                                                                      ])]
            let inputOutOfSyncTransactions = fromList [("08/30/2020", [ Transaction { date = "08/30/2020", amount = 100.00, merchant = "" }
                                                                      , Transaction { date = "08/30/2020", amount = 75.00, merchant = "" }
                                                                      ])]
            let expectedResult = fromList [ ("08/31/2020", TransactionsDiff { tdVerified = []
                                                                            , tdExtra = []
                                                                            , tdMissing = [ Transaction { date = "08/31/2020", amount = 20.01, merchant = "" }
                                                                                          , Transaction { date = "08/31/2020", amount = 54.02, merchant = "" }
                                                                                          , Transaction { date = "08/31/2020", amount = 30.00, merchant = "" }
                                                                                          , Transaction { date = "08/31/2020", amount = 200.00, merchant = "" }
                                                                                          ]
                                                                            })
                                          , ("08/30/2020", TransactionsDiff { tdVerified = []
                                                                            , tdMissing = []
                                                                            , tdExtra = [ Transaction { date = "08/30/2020", amount = 100.00, merchant = "" }
                                                                                        , Transaction { date = "08/30/2020", amount = 75.00, merchant = "" }
                                                                                        ]
                                                                            })
                                          ]
              
            calculateTransactionDiffs inputReferenceTransactions inputOutOfSyncTransactions `shouldBe` expectedResult

        it "should return correct date mapped transaction diffs when given two different date mapped transactions case 4" $ do
            let inputReferenceTransactions = fromList [ ("08/31/2020", [ Transaction { date = "08/31/2020", amount = 20.01, merchant = "" }
                                                                       , Transaction { date = "08/31/2020", amount = 54.02, merchant = "" }
                                                                       , Transaction { date = "08/31/2020", amount = 30.00, merchant = "" }
                                                                       , Transaction { date = "08/31/2020", amount = 200.00, merchant = "" }
                                                                       ])
                                                      , ("08/20/2020", [ Transaction { date = "08/20/2020", amount = 500.00, merchant = "" } ])
                                                      ]
            let inputOutOfSyncTransactions = fromList [ ("08/30/2020", [ Transaction { date = "08/30/2020", amount = 100.00, merchant = "" }
                                                                        , Transaction { date = "08/30/2020", amount = 75.00, merchant = "" }
                                                                        ])
                                                      , ("08/20/2020", [ Transaction { date = "08/20/2020", amount = 500.00, merchant = "" } ])
                                                      ]
            let expectedResult = fromList [ ("08/31/2020", TransactionsDiff { tdVerified = []
                                                                            , tdExtra = []
                                                                            , tdMissing = [ Transaction { date = "08/31/2020", amount = 20.01, merchant = "" }
                                                                                          , Transaction { date = "08/31/2020", amount = 54.02, merchant = "" }
                                                                                          , Transaction { date = "08/31/2020", amount = 30.00, merchant = "" }
                                                                                          , Transaction { date = "08/31/2020", amount = 200.00, merchant = "" }
                                                                                          ]
                                                                            })
                                          , ("08/30/2020", TransactionsDiff { tdVerified = []
                                                                            , tdMissing = []
                                                                            , tdExtra = [ Transaction { date = "08/30/2020", amount = 100.00, merchant = "" }
                                                                                        , Transaction { date = "08/30/2020", amount = 75.00, merchant = "" }
                                                                                        ]
                                                                            })
                                          , ("08/20/2020", TransactionsDiff { tdExtra = []
                                                                            , tdMissing = []
                                                                            , tdVerified = [ Transaction { date = "08/20/2020", amount = 500.00, merchant = "" } ]
                                                                            })
                                          ]
              
            calculateTransactionDiffs inputReferenceTransactions inputOutOfSyncTransactions `shouldBe` expectedResult
