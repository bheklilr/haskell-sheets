{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Data.Sheets.Sheet
    (
    ) where

import Data.Int

import qualified Data.Vector as V
import qualified Data.Text   as T
import Data.HList

import Data.Sheets.Idx (Idx, rangeIdxFrom)
import Data.Sheets.Series (Series, newSeries)


data Sheet idx_elem (col_type :: [*]) = Sheet
    { _name :: [T.Text]
    , _index :: Idx idx_elem
    , _columns :: HList col_type
    }


empty :: Sheet Int32 '[]
empty = Sheet [] (rangeIdxFrom []) HNil
