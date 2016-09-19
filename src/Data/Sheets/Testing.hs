{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Data.Sheets.Testing where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Vector as V

import Text.Read

class Def d where
    defaults :: d


data Sheet idx cols where
    Sheet' :: idx -> cols -> Sheet idx cols

deriving instance (Show idx, Show cols) => Show (Sheet idx cols)
deriving instance (Read idx, Read cols) => Read (Sheet idx cols)

data (name::Symbol):=val_type = Proxy name:=V.Vector val_type deriving (Eq, Show, Read)
infixr 9 :=
data (name::Symbol):=?val_type = Proxy name:=?V.Vector (Maybe val_type) deriving (Eq, Show, Read)
infixr 9 :=?
data left:+right = left:+right deriving (Eq, Show, Read)
infixr 0 :+



data tx:-rx = tx:-rx deriving (Eq, Show, Read)
-- instance (Show tx, Show rx) => Show (tx:-rx) where
--     show (tx:-rx) = show tx ++ "-" ++ show rx
instance (Eq tx, Eq rx, Enum tx, Enum rx) => Enum (tx:-rx) where
    toEnum n = [tx:-rx | tx <- enumFrom (toEnum 0), rx <- enumFrom (toEnum 0)] !! n
    fromEnum a = succ $ length $ takeWhile (/= a) [tx:-rx | tx <- enumFrom (toEnum 0), rx <- enumFrom (toEnum 0)]


data ReadOptions = ReadOptions
    { f::String
    , indexCol::Maybe Int
    , header::Bool
    }
instance Def ReadOptions where
    defaults = ReadOptions "" (Just 0) True

readCSV :: (Read idx, Read cols) => ReadOptions -> IO (Maybe (Sheet idx cols))
readCSV opts = readMaybe <$> readFile (f opts)

type PartNumber =String
type SerialNumber = String
data UTCDateTime = UTCDateTime String deriving (Eq, Read, Show)


data Tx = AB | CD | EF | GH | LM deriving (Eq, Show, Read, Enum, Ord)

data Rx = ML | HG | FE | DC | BA deriving (Eq, Show, Read, Enum, Ord)
type Pair = Tx:-Rx

data Param
    = SDD21
    | SCD21_MOVAVG
    | IPSKEW
    | IPSKEW_DE_EM
    | SEILD_MOVAVG
    | ILD_MOVAVG
    deriving (Eq, Show, Read)

type Region = String


main :: IO ()
main = do
    Just (df :: Sheet ("":=Int) ( "part_number":=PartNumber :+ "serial_number":=SerialNumber :+ "retest":=Int)) <- readCSV defaults { f="./test.csv"}
    print df
