{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data

import Control.Monad
import Data.Int
import Data.Word
import Instances.TH.Lift()
import Language.Haskell.TH.Syntax
import System.Exit
import Test.QuickCheck.All

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set

import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy

import qualified Data.Vector as Vector.Boxed
import qualified Data.Vector.Primitive as Vector.Primitive
import qualified Data.Vector.Storable as Vector.Storable
import qualified Data.Vector.Unboxed as Vector.Unboxed

--------------------------------------------------------------------------------
-- Base
prop_word8 :: Bool
prop_word8 = $(lift (10 :: Word8)) == (10 :: Word8)

prop_word16 :: Bool
prop_word16 = $(lift (10 :: Word16)) == (10 :: Word16)

prop_word32 :: Bool
prop_word32 = $(lift (10 :: Word32)) == (10 :: Word32)

prop_word64 :: Bool
prop_word64 = $(lift (10 :: Word64)) == (10 :: Word64)

prop_int8 :: Bool
prop_int8 = $(lift (10 :: Int8)) == (10 :: Int8)

prop_int16 :: Bool
prop_int16 = $(lift (10 :: Int16)) == (10 :: Int16)

prop_int32 :: Bool
prop_int32 = $(lift (10 :: Int32)) == (10 :: Int32)

prop_int64 :: Bool
prop_int64 = $(lift (10 :: Int64)) == (10 :: Int64)

prop_float :: Bool
prop_float = $(lift (1.1 :: Float)) == (1.1 :: Float)

prop_double :: Bool
prop_double = $(lift (1.1 :: Double)) == (1.1 :: Double)

--------------------------------------------------------------------------------
-- Containers
prop_lazy_int_map :: Bool
prop_lazy_int_map = $(lift $ IntMap.fromList mapdata) == IntMap.fromList mapdata

prop_lazy_map :: Bool
prop_lazy_map = $(lift $ Map.fromList mapdata) == Map.fromList mapdata

prop_int_set :: Bool
prop_int_set = $(lift $ IntSet.fromList setdata) == IntSet.fromList setdata

prop_set :: Bool
prop_set = $(lift $ Set.fromList setdata) == Set.fromList setdata

prop_tree :: Bool
prop_tree = $(lift treedata) == treedata

prop_sequence :: Bool
prop_sequence = $(lift $ Sequence.fromList setdata) == Sequence.fromList setdata

--------------------------------------------------------------------------------
-- Text
prop_text :: Bool
prop_text = $(lift $ Text.pack textdata) == Text.pack textdata

prop_lazy_text :: Bool
prop_lazy_text = $(lift $ Text.Lazy.pack textdata) == Text.Lazy.pack textdata

--------------------------------------------------------------------------------
-- ByteString
prop_bytestring :: Bool
prop_bytestring = $(lift $ ByteString.pack bytedata) == ByteString.pack bytedata

prop_lazy_bytestring :: Bool
prop_lazy_bytestring = $(lift $ ByteString.Lazy.pack bytedata) == ByteString.Lazy.pack bytedata

--------------------------------------------------------------------------------
-- Vector
prop_boxed_vector :: Bool
prop_boxed_vector = $(lift $ Vector.Boxed.fromList bytedata) == Vector.Boxed.fromList bytedata

prop_unboxed_vector :: Bool
prop_unboxed_vector = $(lift $ Vector.Unboxed.fromList bytedata) == Vector.Unboxed.fromList bytedata

prop_primitive_vector :: Bool
prop_primitive_vector = $(lift $ Vector.Primitive.fromList bytedata) == Vector.Primitive.fromList bytedata

prop_storable_vector :: Bool
prop_storable_vector = $(lift $ Vector.Storable.fromList bytedata) == Vector.Storable.fromList bytedata

return []
main :: IO ()
main = do
  success <- $quickCheckAll
  unless success exitFailure
