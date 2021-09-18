module BankAccountTransactionParser.CellRowParser.PureCalculations where

import BankAccountTransactionParser.CellRowParser.DomainModels as DM

import Data.Char
import Data.List.Split
import Data.Text

-----------------Function Declarations-----------------
isNotEmpty :: DM.Cell -> Bool
convertDelimitedToCellRow :: Text -> DM.DelimitedRow -> DM.CellRow

----------------Function Implementations----------------
isNotEmpty cell = case cell of DM.Empty -> False
                               DM.Filled _ -> True

convertDelimitedToCellRow delimiter delimitedRow = let
    splitRow = Data.Text.splitOn delimiter (pack delimitedRow)
    cellRow = Prelude.map (\x -> if Data.Text.all isSpace x then DM.Empty else DM.Filled (unpack x)) splitRow
    in cellRow
