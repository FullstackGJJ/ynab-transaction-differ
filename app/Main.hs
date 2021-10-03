{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (when)
import Data.Char (toUpper)
import System.Environment (getArgs)
import System.Console.Docopt
import Text.Groom

import qualified Library as YTD

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

rowHeaderMap = YTD.RowHeaderMap { YTD.rhmDate = 1, YTD.rhmDescription = 2, YTD.rhmDebit = 3, YTD.rhmCredit = 4 }

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    bearerToken <- args `getArgOrExit` (argument "bearerToken")
    budgetId <- args `getArgOrExit` (argument "budgetId")
    accountId <- args `getArgOrExit` (argument "accountId")
    sinceDate <- args `getArgOrExit` (argument "sinceDate")
    csvFilePath <- args `getArgOrExit` (argument "csvFilePath")
    rowHeaderMapPath <- args `getArgOrExit` (argument "rowHeaderMapPath")

    putStrLn ("Bearer token is: " ++ bearerToken)
    putStrLn ("Budget id is: " ++ budgetId)
    putStrLn ("Account id is: " ++ accountId)
    putStrLn ("Since Date is: " ++ sinceDate)
    putStrLn ("CSV Filepath is: " ++ csvFilePath)
    putStrLn ("Row Header Map Path is: " ++ rowHeaderMapPath)

    let ynabApiParameters = YTD.YnabApiParameters { YTD.bearerToken = bearerToken
                                                  , YTD.budgetId = budgetId
                                                  , YTD.accountId = accountId
                                                  , YTD.sinceDate = sinceDate
                                                  }

    csvFile <- readFile csvFilePath
    let csvLines = lines csvFile
    attempt <- YTD.diffWithBankTransactions ynabApiParameters csvLines rowHeaderMap
    case attempt of Left error -> putStrLn error
                    Right result -> putStrLn (groom result)
