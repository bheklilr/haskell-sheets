module Data.Sheets.Series
    ( Series(..)
    , newSeries
    ) where

import Data.Int
import qualified Data.Foldable as F

import qualified Data.Vector as V
import qualified Data.Text   as T

import Data.Sheets.Idx (Idx, rangeIdxFrom)


data Series idx_elem_type elem_type = Series
    { _names :: [T.Text]
    , _index :: Idx idx_elem_type
    , _values :: V.Vector elem_type
    } deriving
        ( Eq
        , Show
        )

instance Functor (Series idx_elem_type) where
    fmap f (Series names idx values) = Series names idx (fmap f values)

newSeries :: Foldable container => container elem_type -> Series Int32 elem_type
newSeries values = Series [] (rangeIdxFrom values) $ V.fromList . F.toList $ values
