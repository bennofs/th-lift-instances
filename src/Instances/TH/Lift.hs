{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, CPP, StandaloneDeriving, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Instances.TH.Lift
  ( -- | This module provides orphan instances for the 'Language.Haskell.TH.Syntax.Lift' class from template-haskell. Following is a list of the provided instances.
    --
    -- Lift instances are useful to precompute values at compile time using template haskell. For example, if you write the following code,
    -- you can make sure that @3 * 10@ is really computed at compile time:
    --
    -- > {-# LANGUAGE TemplateHaskell #-}
    -- >
    -- > import Language.Haskell.TH.Syntax
    -- >
    -- > expensiveComputation :: Word32
    -- > expensiveComputation = $(lift $ 3 * 10) -- This will computed at compile time
    --
    -- This uses the Lift instance for Word32.
    --
    -- The following instances are provided by this package:

    -- * Base
    -- |  * 'Word8', 'Word16', 'Word32', 'Word64'
    --
    --    * 'Int8', 'Int16', 'Int32', 'Int64'

    -- * Containers (both strict/lazy)
    -- |  * 'Data.IntMap.IntMap'
    --
    --    * 'Data.IntSet.IntSet'
    --
    --    * 'Data.Map.Map'
    --
    --    * 'Data.Set.Set'
    --
    --    * 'Data.Tree.Tree'
    --
    --    * 'Data.Sequence.Seq'

    -- * ByteString (both strict/lazy)
    -- |  * 'Data.ByteString.ByteString'

    -- * Text (both strict/lazy)
    -- |  * 'Data.Text.Text'

    -- * Vector (Boxed, Unboxed, Storable, Primitive)
    -- |  * 'Data.Vector.Vector'

  ) where

import Instances.TH.Lift.Internal
import Language.Haskell.TH.Lift (deriveLift)

-- Base
import Data.Complex
import qualified Data.Foldable as F
import Data.Int
import Data.Word
import Language.Haskell.TH.Syntax (Lift(..))

#if !MIN_VERSION_template_haskell(2,9,1)
import Data.Ratio
import Language.Haskell.TH
#endif

-- Containers
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import qualified Data.Tree as Tree

-- Text
import qualified Data.Text as Text
import qualified Data.Text.Array as Text
import qualified Data.Text.Lazy as Text.Lazy

-- ByteString
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy

-- ByteArray
import Data.Primitive.ByteArray (ByteArray)

-- Vector
import qualified Data.Vector as Vector.Boxed
import qualified Data.Vector.Primitive as Vector.Primitive
import qualified Data.Vector.Storable as Vector.Storable
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Base as UV

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
#if !MIN_VERSION_template_haskell(2,9,1)
-- Base

instance Lift Int8 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int16 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int32 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int64 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word8 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word16 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word32 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word64 where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Integral a => Lift (Ratio a) where
  lift x = return (LitE (RationalL (toRational x)))

instance Lift Float where
  lift x = [| $(litE $ rationalL $ toRational x) :: Float |]

instance Lift Double where
  lift x = [| $(litE $ rationalL $ toRational x) :: Double |]

# endif

--------------------------------------------------------------------------------
-- Containers
instance Lift v => Lift (IntMap.IntMap v) where
  lift m = [| IntMap.fromList $(lift $ IntMap.toList m) |]

instance Lift IntSet.IntSet where
  lift s = [| IntSet.fromList $(lift $ IntSet.toList s) |]

instance (Lift k, Lift v) => Lift (Map.Map k v) where
  lift m = [| Map.fromList $(lift $ Map.toList m) |]

instance Lift a => Lift (Sequence.Seq a) where
  lift s = [| Sequence.fromList $(lift $ F.toList s) |]

instance Lift a => Lift (Set.Set a) where
  lift s = [| Set.fromList $(lift $ Set.toList s) |]

deriveLift ''Tree.Tree

--------------------------------------------------------------------------------
-- Text
instance Lift Text.Text where lift = liftText

instance Lift Text.Lazy.Text where
  lift lt = [| Text.Lazy.fromStrict $(lift (Text.Lazy.toStrict lt)) |]

instance Lift Text.Array where lift = liftTextArray

--------------------------------------------------------------------------------
-- ByteArray

instance Lift ByteArray where lift = liftByteArray

--------------------------------------------------------------------------------
-- ByteString
instance Lift ByteString.ByteString where lift = liftByteString

instance Lift ByteString.Lazy.ByteString where
  lift b = [| ByteString.Lazy.fromStrict $(lift (ByteString.Lazy.toStrict b)) |]

--------------------------------------------------------------------------------
-- Vector
instance Vector.Primitive.Prim a => Lift (Vector.Primitive.Vector a) where
  lift = liftPrimitiveVector

instance Vector.Storable.Storable a => Lift (Vector.Storable.Vector a) where
  lift = liftStorableVector

instance Lift (UV.Vector Bool)   where lift (UV.V_Bool   v) = [| UV.V_Bool   v |]
instance Lift (UV.Vector Char)   where lift (UV.V_Char   v) = [| UV.V_Char   v |]
instance Lift (UV.Vector Double) where lift (UV.V_Double v) = [| UV.V_Double v |]
instance Lift (UV.Vector Float)  where lift (UV.V_Float  v) = [| UV.V_Float  v |]
instance Lift (UV.Vector Int)    where lift (UV.V_Int    v) = [| UV.V_Int    v |]
instance Lift (UV.Vector Int8)   where lift (UV.V_Int8   v) = [| UV.V_Int8   v |]
instance Lift (UV.Vector Int16)  where lift (UV.V_Int16  v) = [| UV.V_Int16  v |]
instance Lift (UV.Vector Int32)  where lift (UV.V_Int32  v) = [| UV.V_Int32  v |]
instance Lift (UV.Vector Int64)  where lift (UV.V_Int64  v) = [| UV.V_Int64  v |]
instance Lift (UV.Vector Word)   where lift (UV.V_Word   v) = [| UV.V_Word   v |]
instance Lift (UV.Vector Word8)  where lift (UV.V_Word8  v) = [| UV.V_Word8  v |]
instance Lift (UV.Vector Word16) where lift (UV.V_Word16 v) = [| UV.V_Word16 v |]
instance Lift (UV.Vector Word32) where lift (UV.V_Word32 v) = [| UV.V_Word32 v |]
instance Lift (UV.Vector ())     where lift (UV.V_Unit   v) = [| UV.V_Unit   v |]

instance Lift (UV.Vector a) => Lift (UV.Vector (Complex a)) where
  lift (UV.V_Complex v) = [| UV.V_Complex v |]

instance (Lift (UV.Vector a), Lift (UV.Vector b))
  => Lift (UV.Vector (a, b)) where
  lift (UV.V_2 l a b) = [| UV.V_2 l a b |]

instance (Lift (UV.Vector a), Lift (UV.Vector b), Lift (UV.Vector c))
  => Lift (UV.Vector (a, b, c)) where
  lift (UV.V_3 l a b c) = [| UV.V_3 l a b c |]

instance (Lift (UV.Vector a), Lift (UV.Vector b), Lift (UV.Vector c), Lift (UV.Vector d))
  => Lift (UV.Vector (a, b, c, d)) where
  lift (UV.V_4 l a b c d) = [| UV.V_4 l a b c d |]

instance (Lift (UV.Vector a), Lift (UV.Vector b), Lift (UV.Vector c), Lift (UV.Vector d), Lift (UV.Vector e))
  => Lift (UV.Vector (a, b, c, d, e)) where
  lift (UV.V_5 l a b c d e) = [| UV.V_5 l a b c d e |]

instance (Lift (UV.Vector a), Lift (UV.Vector b), Lift (UV.Vector c), Lift (UV.Vector d), Lift (UV.Vector e), Lift (UV.Vector f))
  => Lift (UV.Vector (a, b, c, d, e, f)) where
  lift (UV.V_6 l a b c d e f) = [| UV.V_6 l a b c d e f |]

instance Lift a => Lift (Vector.Boxed.Vector a) where
  lift v = [| Vector.Boxed.fromList $(lift $ Vector.Boxed.toList v) |]
