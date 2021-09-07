{-# LANGUAGE OverloadedStrings #-}

module YnabApi.InternalToActions where

import qualified YnabApi.DomainModels as DM
import qualified YnabApi.DomainRules as DR

import Control.Exception
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.ByteString.Char8
import Network.Wreq

-----------------Function Declarations-----------------
getTransactionsFromApiM :: String -> String -> String -> String -> IO (Either String [DM.TransactionDetail])

----------------Function Implementations----------------
getTransactionsFromApiM bearerToken budgetId accountId sinceDate = do
    let bearerHeader = pack("Bearer " ++ bearerToken)
    let opts = defaults & header "Authorization" .~ [bearerHeader]
    let url = "https://api.youneedabudget.com/v1/budgets/" ++ budgetId ++ "/accounts/" ++ accountId ++ "/transactions?since_date=" ++ sinceDate
    response <- try (asJSON =<< getWith opts url) :: IO (Either SomeException (Response DM.TransactionResponse))
    case response of Left _ -> return (Left "Error in making API request")
                     Right r -> return (Right (DM.transactions (DM.trData (r ^. responseBody))))

instance FromJSON DM.TransactionDetail where
    parseJSON = withObject "TransactionDetail" $ \obj -> do
        date <- obj .: "date"
        amount <- obj .: "amount"
        payeeId <- obj .: "payee_id"
        return DM.TransactionDetail { DM.date = date, DM.amount = amount, DM.payeeId = payeeId }

instance FromJSON DM.TransactionsData

instance FromJSON DM.TransactionResponse where
    parseJSON = withObject "TransactionResponse" $ \obj -> do
        trData <- obj .: "data"
        return DM.TransactionResponse { DM.trData = trData }
