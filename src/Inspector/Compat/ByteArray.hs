{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan 'Data.ByteArray' instances for "Basement"/"Foundation" types.
--
-- The @ram@ package (a maintained fork of the now-unmaintained @memory@
-- package) only ships 'ByteArrayAccess'\/'ByteArray' instances for
-- 'Data.ByteString.ByteString'. The old @memory@ package additionally
-- provided instances for 'Basement.UArray.UArray', 'Basement.Block.Block'
-- and Foundation's 'String' (gated behind its @foundation@ cabal flag).
-- The rest of this codebase converts between 'Digest'\/'Bytes' and those
-- Foundation byte-array types, so this module restores the missing
-- instances to keep it compiling against @ram@.
module Inspector.Compat.ByteArray () where

import Foundation hiding (length)

import qualified Basement.Block as Block
import qualified Basement.Block.Mutable as Block
import qualified Basement.String as Base (Encoding (UTF8), String, toBytes)
import qualified Basement.Types.OffsetSize as Base (CountOf (..))
import qualified Basement.UArray as Base

import Foreign.Ptr (castPtr)

import Data.ByteArray (ByteArray (..), ByteArrayAccess (..))

sizeRecastBytes :: Base.PrimType ty => Int -> Proxy ty -> Base.CountOf ty
sizeRecastBytes w p = Base.CountOf $
    let (q, r) = w `divMod` szTy
     in q + (if r == 0 then 0 else 1)
  where
    !(Base.CountOf szTy) = Base.primSizeInBytes p

baseUarrayRecastW8 :: Base.PrimType ty => Base.UArray ty -> Base.UArray Word8
baseUarrayRecastW8 = Base.recast

instance Base.PrimType ty => ByteArrayAccess (Base.UArray ty) where
    length a = let Base.CountOf i = Base.length (baseUarrayRecastW8 a) in i
    withByteArray a f = Base.withPtr (baseUarrayRecastW8 a) (f . castPtr)

instance (Ord ty, Base.PrimType ty) => ByteArray (Base.UArray ty) where
    allocRet sz f = do
        mba <- Base.new (sizeRecastBytes sz Proxy)
        a   <- Base.withMutablePtr mba (f . castPtr)
        ba  <- Base.unsafeFreeze mba
        pure (a, ba)

instance ByteArrayAccess Base.String where
    length str = let Base.CountOf i = Base.length bytes in i
      where
        -- Foundation's 'length' returns a number of elements, not bytes.
        bytes = Base.toBytes Base.UTF8 str
    withByteArray s f = withByteArray (Base.toBytes Base.UTF8 s) f

baseBlockRecastW8 :: Base.PrimType ty => Block.Block ty -> Block.Block Word8
baseBlockRecastW8 = Block.unsafeCast

instance Base.PrimType ty => ByteArrayAccess (Block.Block ty) where
    length a = let Base.CountOf i = Block.length (baseBlockRecastW8 a) in i
    withByteArray a f = Block.withPtr (baseBlockRecastW8 a) (f . castPtr)

instance (Ord ty, Base.PrimType ty) => ByteArray (Block.Block ty) where
    allocRet sz f = do
        mba <- Block.new (sizeRecastBytes sz Proxy)
        a   <- Block.withMutablePtr mba (f . castPtr)
        ba  <- Block.unsafeFreeze mba
        pure (a, ba)
