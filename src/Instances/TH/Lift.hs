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
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import qualified Data.Tree as Tree

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

-- support typed template haskell (requires template-haskell>=1.16)
#if MIN_VERSION_template_haskell(2,16,0)
#define LIFT liftTyped
#define QUOTE(x) [|| (x) ||]
#define TYPED_RETURN unsafeTExpCoerce . return
#else
#define LIFT lift
#define QUOTE(x) [| (x) |]
#define TYPED_RETURN return
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
instance Lift v => Lift (IntMap.IntMap v) where
  LIFT m = QUOTE(IntMap.fromDistinctAscList ml) where
    ml = IntMap.toAscList m

instance Lift IntSet.IntSet where
  LIFT s = QUOTE(IntSet.fromDistinctAscList sl) where
    sl = IntSet.toAscList s

instance (Lift k, Lift v) => Lift (Map.Map k v) where
  LIFT m = QUOTE(Map.fromDistinctAscList ml) where
    ml = Map.toAscList m

instance Lift a => Lift (Sequence.Seq a) where
  LIFT s = QUOTE(Sequence.fromList sl) where
    sl = F.toList s

instance Lift a => Lift (Set.Set a) where
  LIFT s = QUOTE(Set.fromDistinctAscList sl) where
    sl = Set.toAscList s

instance Lift a => Lift (Tree.Tree a) where
  LIFT (Tree.Node x xs) = QUOTE(Tree.Node x xs)

#if !MIN_VERSION_text(1,2,4)
--------------------------------------------------------------------------------
-- Text
instance Lift Text.Text where
  LIFT t = QUOTE(Text.pack tp) where
    tp = Text.unpack t

instance Lift Text.Lazy.Text where
  LIFT t = QUOTE(Text.Lazy.pack tp) where
    tp = Text.Lazy.unpack t
#endif

--------------------------------------------------------------------------------
-- ByteString
instance Lift ByteString.ByteString where
  -- this is essentially what e.g. file-embed does
  LIFT b = TYPED_RETURN $ AppE (VarE 'unsafePerformIO) $
    VarE 'ByteString.Unsafe.unsafePackAddressLen `AppE` l `AppE` b'
    where
      l  = LitE $ IntegerL $ fromIntegral $ ByteString.length b
      b' =
#if MIN_VERSION_template_haskell(2, 8, 0)
        LitE $ StringPrimL $ ByteString.unpack b
#else
        LitE $ StringPrimL $ ByteString.Char8.unpack b
#endif

instance Lift ByteString.Lazy.ByteString where
  LIFT lb = QUOTE(ByteString.Lazy.fromChunks bc) where
    bc = ByteString.Lazy.toChunks lb

--------------------------------------------------------------------------------
-- Vector
instance (Vector.Primitive.Prim a, Lift a) => Lift (Vector.Primitive.Vector a) where
  LIFT v = QUOTE(Vector.Primitive.fromListN np vp) where
    np = Vector.Primitive.length v
    vp = Vector.Primitive.toList v

instance (Vector.Storable.Storable a, Lift a) => Lift (Vector.Storable.Vector a) where
  LIFT v = QUOTE(Vector.Storable.fromListN np vp) where
    np = Vector.Storable.length v
    vp = Vector.Storable.toList v

instance (Vector.Unboxed.Unbox a, Lift a) => Lift (Vector.Unboxed.Vector a) where
  LIFT v = QUOTE(Vector.Unboxed.fromListN np vp) where
    np = Vector.Unboxed.length v
    vp = Vector.Unboxed.toList v

instance Lift a => Lift (Vector.Boxed.Vector a) where
  LIFT v = QUOTE(Vector.Boxed.fromListN np vp) where
    np = Vector.Boxed.length v
    vp = Vector.Boxed.toList v

--------------------------------------------------------------------------------
-- Transformers
instance Lift a => Lift (Identity a) where
  LIFT (Identity a) = QUOTE(Identity a)

instance Lift a => Lift (Const a b) where
  LIFT (Const a) = QUOTE(Const a)
