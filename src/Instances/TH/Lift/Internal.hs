{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TemplateHaskell #-}

module Instances.TH.Lift.Internal
    ( liftText
    , liftTextArray
    , liftByteArray
    , liftByteArrayPinned
    , liftByteString
    , liftPrimitiveVector
    , liftStorableVector
    ) where

import           Control.Monad
import           Control.Monad.Primitive (PrimMonad (..))
import           Control.Monad.ST
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import           Data.Primitive.ByteArray
import qualified Data.Text.Array as TArray
import qualified Data.Text.Internal as Text
import qualified Data.Vector.Primitive as Vector.Primitive
import qualified Data.Vector.Storable as Vector.Storable
import           Data.Word (Word8)
import           Foreign.Storable
import           Foreign.ForeignPtr
import           GHC.Prim (Addr#, copyAddrToByteArray#, sizeofByteArray#)
import           GHC.Ptr (Ptr(..), castPtr)
import           GHC.Types (IO(..), Int(..))
import           Language.Haskell.TH
import           System.IO.Unsafe (unsafePerformIO)

liftText :: Text.Text -> ExpQ
liftText (Text.Text arr0 off len) =
    [| unsafePerformIO $ do
        ByteArray ba <- addrToByteArray sz $(addrQ)
        return $ Text.Text (TArray.Array ba) 0 len |]
  where
    addrQ = addrBytesExpQ (unpackByteArrayIvl ba0 off sz)
    ba0 = ByteArray (TArray.aBA arr0)
    sz = len * 2

liftTextArray :: TArray.Array -> ExpQ
liftTextArray arr =
    [| unsafePerformIO $ addrToTextArray len $(addrBytesExpQ (unpackTextArray arr)) |]
  where
    len = sz `div` 2
    sz = I# (sizeofByteArray# (TArray.aBA arr))

liftByteArray :: ByteArray -> ExpQ
liftByteArray arr =
    [| unsafePerformIO $ addrToByteArray sz $(addrBytesExpQ (unpackByteArray arr)) |]
  where
    sz = sizeofByteArray arr

liftByteArrayPinned :: ByteArray -> ExpQ
liftByteArrayPinned arr =
    [| unsafePerformIO $ addrToPinnedByteArray sz $(addrBytesExpQ (unpackByteArray arr)) |]
  where
    sz = sizeofByteArray arr

liftByteString :: ByteString.ByteString -> ExpQ
liftByteString bs =
    [| unsafePerformIO $ ByteString.unsafePackAddressLen sz $(addrBytesExpQ (ByteString.unpack bs)) |]
  where
    sz = ByteString.length bs

liftPrimitiveVector :: Vector.Primitive.Prim a => Vector.Primitive.Vector a -> ExpQ
liftPrimitiveVector (Vector.Primitive.Vector off len ba0) =
    [| unsafePerformIO $ do
        ba <- addrToByteArray sz $(addrBytesExpQ (unpackByteArrayIvl ba0 off len))
        return $ Vector.Primitive.Vector 0 len ba |]
  where
    sz = sizeofByteArray ba0

liftStorableVector :: Vector.Storable.Storable a => Vector.Storable.Vector a -> ExpQ
liftStorableVector vec = do
    bytes <- runIO (unpackForeignPtr fptr0 len)
    [| unsafePerformIO $ do
        fptr <- newForeignPtr_ (Ptr $(addrBytesExpQ bytes))
        return $ Vector.Storable.unsafeFromForeignPtr0 fptr len |]
  where
    (fptr0, len) = Vector.Storable.unsafeToForeignPtr0 vec

addrBytesExpQ :: [Word8] -> ExpQ
addrBytesExpQ = litE . stringPrimL

unpackTextArray :: TArray.Array -> [Word8]
unpackTextArray arr = unpackByteArray (ByteArray (TArray.aBA arr))

unpackByteArray :: ByteArray -> [Word8]
unpackByteArray ba = unpackByteArrayIvl ba 0 (sizeofByteArray ba)

unpackByteArrayIvl :: ByteArray -> Int -> Int -> [Word8]
unpackByteArrayIvl ba off len = map (indexByteArray ba) [off..(off + len - 1)]

unpackForeignPtr :: ForeignPtr a -> Int -> IO [Word8]
unpackForeignPtr fptr len =
    withForeignPtr fptr $ \ptr ->
        forM [0..len-1] (peekElemOff (castPtr ptr))

addrToTextArray :: Int -> Addr# -> IO TArray.Array
addrToTextArray w16sz addr = do
  mta <- stToIO (TArray.new w16sz)
  copyAddrToByteArray (Ptr addr) (MutableByteArray (TArray.maBA mta)) 0 (w16sz * 2)
  stToIO (TArray.unsafeFreeze mta)

addrToByteArray :: Int -> Addr# -> IO ByteArray
addrToByteArray sz addr = do
  mba <- newByteArray sz
  copyAddrToByteArray (Ptr addr) mba 0 sz
  unsafeFreezeByteArray mba

addrToPinnedByteArray :: Int -> Addr# -> IO ByteArray
addrToPinnedByteArray sz addr = do
  mba <- newPinnedByteArray sz
  copyAddrToByteArray (Ptr addr) mba 0 sz
  unsafeFreezeByteArray mba

copyAddrToByteArray :: Ptr a -> MutableByteArray (PrimState IO) -> Int -> Int -> IO ()
copyAddrToByteArray (Ptr addr) (MutableByteArray arr) (I# offset) (I# len) =
    IO (\s -> (# copyAddrToByteArray# addr arr offset len s, () #))
