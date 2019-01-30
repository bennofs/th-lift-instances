{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
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

import qualified Data.Foldable as F

-- Base
#if !MIN_VERSION_template_haskell(2,9,1)
import Language.Haskell.TH
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
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import qualified Data.Tree as Tree

-- Text
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy

-- ByteString
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy

-- Vector
import qualified Data.Vector as Vector.Boxed
import qualified Data.Vector.Primitive as Vector.Primitive
import qualified Data.Vector.Storable as Vector.Storable
import qualified Data.Vector.Unboxed as Vector.Unboxed


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
instance Lift v => Lift (IntMap.IntMap v) where
  lift m = [| IntMap.fromList m' |] where
    m' = IntMap.toList m

instance Lift IntSet.IntSet where
  lift s = [| IntSet.fromList s' |] where
    s' = IntSet.toList s

instance (Lift k, Lift v) => Lift (Map.Map k v) where
  lift m = [| Map.fromList m' |] where
    m' = Map.toList m

instance Lift a => Lift (Sequence.Seq a) where
  lift s = [| Sequence.fromList s' |] where
    s' = F.toList s

instance Lift a => Lift (Set.Set a) where
  lift s = [| Set.fromList s' |] where
    s' = Set.toList s

instance Lift a => Lift (Tree.Tree a) where
  lift (Tree.Node x xs) = [| Tree.Node x xs |]

--------------------------------------------------------------------------------
-- Text
instance Lift Text.Text where
  lift t = [| Text.pack t' |] where
    t' = Text.unpack t

instance Lift Text.Lazy.Text where
  lift t = [| Text.Lazy.pack t' |] where
    t' = Text.Lazy.unpack t

--------------------------------------------------------------------------------
-- ByteString
instance Lift ByteString.ByteString where
  lift b = [| ByteString.pack b' |] where
    b' = ByteString.unpack b

instance Lift ByteString.Lazy.ByteString where
  lift b = [| ByteString.Lazy.pack b' |] where
    b' = ByteString.Lazy.unpack b

--------------------------------------------------------------------------------
-- Vector
instance (Vector.Primitive.Prim a, Lift a) => Lift (Vector.Primitive.Vector a) where
  lift v = [| Vector.Primitive.fromList v' |] where
    v' = Vector.Primitive.toList v

instance (Vector.Storable.Storable a, Lift a) => Lift (Vector.Storable.Vector a) where
  lift v = [| Vector.Storable.fromList v' |] where
    v' = Vector.Storable.toList v

instance (Vector.Unboxed.Unbox a, Lift a) => Lift (Vector.Unboxed.Vector a) where
  lift v = [| Vector.Unboxed.fromList v' |] where
    v' = Vector.Unboxed.toList v

instance Lift a => Lift (Vector.Boxed.Vector a) where
  lift v = [| Vector.Boxed.fromList v' |] where
    v' = Vector.Boxed.toList v
