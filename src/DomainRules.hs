module DomainRules where

import qualified DomainModels as DM

import Data.List
import Data.Map

-----------------Function Declarations-----------------
containsMatchingDates :: DM.DateMappedTransactionCollection -> DM.DateMappedTransactionCollection -> Bool
populateMissingDates :: DM.DateMappedTransactionCollection -> DM.DateMappedTransactionCollection -> DM.DateMappedTransactionCollection

----------------Function Implementations----------------
containsMatchingDates collection1 collection2 = let
    collection1Keys = keys collection1
    collection2Keys = keys collection2
    in all (\x -> member x collection1) collection2Keys && all (\x -> member x collection2) collection1Keys

populateMissingDates referenceCollection targetCollection = 
    Data.List.foldl (\acc key -> if member key acc then acc else Data.Map.insert key [] acc) targetCollection (keys referenceCollection)
