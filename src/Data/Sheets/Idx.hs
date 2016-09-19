{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Sheets.Idx
    ( Idx(..)
    , RangeIdx
    , newIdx
    , rangeIdxFrom
    , named
    , named'
    , withValues
    ) where

import Data.Int
import qualified Data.Foldable as F

import qualified Data.Vector as V
import qualified Data.Text   as T


data Idx elem_type = Idx
    { _names :: [T.Text]
    , _values :: V.Vector elem_type
    } deriving
        ( Eq
        , Show
        , Functor
        , Foldable
        , Traversable
        )

type RangeIdx = Idx Int32

newIdx :: Foldable container => container elem_type -> Idx elem_type
newIdx = Idx [] . V.fromList . F.toList

rangeIdxFrom :: Foldable container => container something -> RangeIdx
rangeIdxFrom = newIdx . V.enumFromN 0 . length

named :: [T.Text] -> Idx elem_type -> Idx elem_type
named new_names idx = idx { _names = new_names }

named' :: T.Text -> Idx elem_type -> Idx elem_type
named' new_name idx = idx { _names = [new_name] }

withValues :: V.Vector new_elem_type -> Idx old_elem_type -> Idx new_elem_type
withValues new_values (Idx names _) = Idx names new_values
