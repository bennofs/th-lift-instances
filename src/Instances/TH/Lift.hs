{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif
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
    --
    --    * 'NonEmpty' and 'Void', until provided by @template-haskell-2.15@

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

import Language.Haskell.TH.Syntax (Lift(..))
#if MIN_VERSION_template_haskell(2,16,0)
import Language.Haskell.TH.Syntax (unsafeTExpCoerce)
#endif
import Language.Haskell.TH

import qualified Data.Foldable as F

-- Base
#if !MIN_VERSION_template_haskell(2,9,1)
import Data.Int
import Data.Word
#endif

#if !MIN_VERSION_template_haskell(2,10,0)
import Data.Ratio (Ratio)
#endif

#if !MIN_VERSION_template_haskell(2,15,0)
#if MIN_VERSION_base(4,8,0)
import Data.Void (Void, absurd)
#endif
#if MIN_VERSION_base(4,9,0)
import Data.List.NonEmpty (NonEmpty (..))
#endif
#endif

-- Containers
import qualified Data.Tree as Tree

#if MIN_VERSION_containers(0,5,10)
-- recent enough containers exports internals,
-- so we can use DeriveLift
-- This way we construct the data type exactly as we have it
-- during compile time, so there is nothing left for run-time.
#define HAS_CONTAINERS_INTERNALS 1

import qualified Data.IntMap.Internal as IntMap
import qualified Data.IntSet.Internal as IntSet
import qualified Data.Map.Internal as Map
import qualified Data.Sequence.Internal as Sequence
import qualified Data.Set.Internal as Set
#else
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
#endif

#if !MIN_VERSION_text(1,2,4)
-- Text
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
#endif

-- ByteString
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString.Unsafe
import qualified Data.ByteString.Lazy as ByteString.Lazy
import           System.IO.Unsafe (unsafePerformIO)
#if !MIN_VERSION_template_haskell(2, 8, 0)
import qualified Data.ByteString.Char8 as ByteString.Char8
#endif

-- Vector
import qualified Data.Vector as Vector.Boxed
import qualified Data.Vector.Primitive as Vector.Primitive
import qualified Data.Vector.Storable as Vector.Storable
import qualified Data.Vector.Unboxed as Vector.Unboxed

-- transformers (or base)
import Control.Applicative (Const (..))
import Data.Functor.Identity (Identity (..))

#if MIN_VERSION_template_haskell(2,17,0)
#define LIFT_TYPED_DEFAULT liftTyped = Code . unsafeTExpCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
#define LIFT_TYPED_DEFAULT liftTyped = unsafeTExpCoerce . lift
#else
#define LIFT_TYPED_DEFAULT
#endif

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
#if !MIN_VERSION_template_haskell(2,9,1)
-- Base

instance Lift Word8 where
  lift x = [| fromInteger x' :: Word8 |] where
    x' = toInteger x

instance Lift Word16 where
  lift x = [| fromInteger x' :: Word16 |] where
    x' = toInteger x

instance Lift Word32 where
  lift x = [| fromInteger x' :: Word32 |] where
    x' = toInteger x

instance Lift Word64 where
  lift x = [| fromInteger x' :: Word64 |] where
    x' = toInteger x

instance Lift Int8 where
  lift x = [| fromInteger x' :: Int8 |] where
    x' = toInteger x

instance Lift Int16 where
  lift x = [| fromInteger x' :: Int16 |] where
    x' = toInteger x

instance Lift Int32 where
  lift x = [| fromInteger x' :: Int32 |] where
    x' = toInteger x

instance Lift Int64 where
  lift x = [| fromInteger x' :: Int64 |] where
    x' = toInteger x

instance Lift Float where
  lift x = return (LitE (RationalL (toRational x)))

instance Lift Double where
  lift x = return (LitE (RationalL (toRational x)))
# endif

#if !MIN_VERSION_template_haskell(2,10,0)
instance Lift () where
  lift () = [| () |]

instance Integral a => Lift (Ratio a) where
  lift x = return (LitE (RationalL (toRational x)))
#endif

#if !MIN_VERSION_template_haskell(2,15,0)
#if MIN_VERSION_base(4,8,0)

instance Lift Void where
    lift = absurd

#endif
#if MIN_VERSION_base(4,9,0)
instance Lift a => Lift (NonEmpty a) where
    lift (x :| xs) = [| x :| xs |]
#endif
#endif

--------------------------------------------------------------------------------
-- Containers
--

#if __GLASGOW_HASKELL__ >= 800
deriving instance Lift a => Lift (Tree.Tree a)
#else
instance Lift a => Lift (Tree.Tree a) where
  lift (Tree.Node x xs) = [| Tree.Node x xs |]
  LIFT_TYPED_DEFAULT
#endif

#if __GLASGOW_HASKELL__ >= 800
deriving instance Lift a => Lift (Sequence.ViewL a)
deriving instance Lift a => Lift (Sequence.ViewR a)
#else
instance Lift a => Lift (Sequence.ViewL a) where
  lift Sequence.EmptyL = [| Sequence.EmptyL |]
  lift (x Sequence.:< xs) = [| x Sequence.:< xs |]
  LIFT_TYPED_DEFAULT
instance Lift a => Lift (Sequence.ViewR a) where
  lift Sequence.EmptyR = [| Sequence.EmptyR |]
  lift (xs Sequence.:> x) = [| xs Sequence.:> x |]
  LIFT_TYPED_DEFAULT
#endif

#if HAS_CONTAINERS_INTERNALS
deriving instance Lift v => Lift (IntMap.IntMap v)
deriving instance Lift IntSet.IntSet
deriving instance (Lift k, Lift v) => Lift (Map.Map k v)
deriving instance Lift a => Lift (Set.Set a)

deriving instance Lift a => Lift (Sequence.Seq a)
deriving instance Lift a => Lift (Sequence.Elem a)
deriving instance Lift a => Lift (Sequence.Digit a)
deriving instance Lift a => Lift (Sequence.Node a)
deriving instance Lift a => Lift (Sequence.FingerTree a)
#else
instance Lift v => Lift (IntMap.IntMap v) where
  lift m = [| IntMap.fromDistinctAscList m' |] where
    m' = IntMap.toAscList m
  LIFT_TYPED_DEFAULT

instance Lift IntSet.IntSet where
  lift s = [| IntSet.fromList s' |] where
    s' = IntSet.toList s
  LIFT_TYPED_DEFAULT

instance (Lift k, Lift v) => Lift (Map.Map k v) where
  lift m = [| Map.fromDistinctAscList m' |] where
    m' = Map.toAscList m
  LIFT_TYPED_DEFAULT

instance Lift a => Lift (Sequence.Seq a) where
  lift s = [| Sequence.fromList s' |] where
    s' = F.toList s
  LIFT_TYPED_DEFAULT

instance Lift a => Lift (Set.Set a) where
  lift s = [| Set.fromDistinctAscList s' |] where
    s' = Set.toAscList s
  LIFT_TYPED_DEFAULT
#endif

#if !MIN_VERSION_text(1,2,4)
--------------------------------------------------------------------------------
-- Text
instance Lift Text.Text where
  lift t = [| Text.pack t' |] where
    t' = Text.unpack t
  LIFT_TYPED_DEFAULT

instance Lift Text.Lazy.Text where
  lift t = [| Text.Lazy.pack t' |] where
    t' = Text.Lazy.unpack t
  LIFT_TYPED_DEFAULT
#endif

--------------------------------------------------------------------------------
-- ByteString
instance Lift ByteString.ByteString where
  -- this is essentially what e.g. file-embed does
  lift b = return $ AppE (VarE 'unsafePerformIO) $
    VarE 'ByteString.Unsafe.unsafePackAddressLen `AppE` l `AppE` b'
    where
      l  = LitE $ IntegerL $ fromIntegral $ ByteString.length b
      b' =
#if MIN_VERSION_template_haskell(2, 8, 0)
        LitE $ StringPrimL $ ByteString.unpack b
#else
        LitE $ StringPrimL $ ByteString.Char8.unpack b
#endif
  LIFT_TYPED_DEFAULT

instance Lift ByteString.Lazy.ByteString where
  lift lb = do
    b' <- lift b
    return  (VarE 'ByteString.Lazy.fromChunks `AppE` b')
    where
      b = ByteString.Lazy.toChunks lb
  LIFT_TYPED_DEFAULT

--------------------------------------------------------------------------------
-- Vector
instance (Vector.Primitive.Prim a, Lift a) => Lift (Vector.Primitive.Vector a) where
  lift v = [| Vector.Primitive.fromListN n' v' |] where
    n' = Vector.Primitive.length v
    v' = Vector.Primitive.toList v
  LIFT_TYPED_DEFAULT

instance (Vector.Storable.Storable a, Lift a) => Lift (Vector.Storable.Vector a) where
  lift v = [| Vector.Storable.fromListN n' v' |] where
    n' = Vector.Storable.length v
    v' = Vector.Storable.toList v
  LIFT_TYPED_DEFAULT

instance (Vector.Unboxed.Unbox a, Lift a) => Lift (Vector.Unboxed.Vector a) where
  lift v = [| Vector.Unboxed.fromListN n' v' |] where
    n' = Vector.Unboxed.length v
    v' = Vector.Unboxed.toList v
  LIFT_TYPED_DEFAULT

instance Lift a => Lift (Vector.Boxed.Vector a) where
  lift v = [| Vector.Boxed.fromListN n' v' |] where
    n' = Vector.Boxed.length v
    v' = Vector.Boxed.toList v
  LIFT_TYPED_DEFAULT

--------------------------------------------------------------------------------
-- Transformers

#if __GLASGOW_HASKELL__ >= 800
deriving instance Lift a => Lift (Identity a)
deriving instance Lift a => Lift (Const a b)
#else
instance Lift a => Lift (Identity a) where
  lift (Identity a) = [| Identity a |]

instance Lift a => Lift (Const a b) where
  lift (Const a) = [| Const a |]
#endif
