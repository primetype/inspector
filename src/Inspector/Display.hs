
module Inspector.Display
    ( Display (..)
    , displayByteArrayAccess
    ) where

import Foundation
import Foundation.String (fromBytesUnsafe)

import Data.ByteArray (ByteArrayAccess, Bytes, ScrubbedBytes)
import Data.ByteArray.Encoding (convertToBase, Base (Base16))

import Crypto.Hash (Digest, digestFromByteString, HashAlgorithm)
import Crypto.MAC.HMAC (HMAC, hmacGetDigest)

class Display a where
    display  :: a -> String
    encoding :: Proxy a -> String
    comment :: Proxy a -> Maybe String
    comment _ = Nothing

instance Display Bool where
    display True = "true"
    display False = "false"
    encoding _ = "either `true` or `false`"
instance Display Int where
    display = show
    encoding _ = "base 10"
instance Display Int16 where
    display = show
    encoding _ = "base 10"
instance Display Int32 where
    display = show
    encoding _ = "base 10"
instance Display Int64 where
    display = show
    encoding _ = "base 10"
instance Display Word where
    display = show
    encoding _ = "base 10"
instance Display Word16 where
    display = show
    encoding _ = "base 10"
instance Display Word32 where
    display = show
    encoding _ = "base 10"
instance Display Word64 where
    display = show
    encoding _ = "base 10"
instance Display Integer where
    display = show
    encoding _ = "base 10"
instance Display Natural where
    display = show
    encoding _ = "base 10"
instance Display Double where
    display = show
    encoding _ = ""
instance Display String where
    display str = "\"" <> str <> "\""
    encoding _ = "UTF8 String"
instance Display Bytes where
    display = displayByteArrayAccess
    encoding _ = "hexadecimal"
instance Display ScrubbedBytes where
    display = displayByteArrayAccess
    encoding _ = "hexadecimal"
instance PrimType ty => Display (UArray ty) where
    display = displayByteArrayAccess
    encoding _ = "hexadecimal"
instance HashAlgorithm hash => Display (Digest hash) where
    display = displayByteArrayAccess
    encoding _ = "hexadecimal"
instance HashAlgorithm hash => Display (HMAC hash) where
    display = displayByteArrayAccess . hmacGetDigest
    encoding _ = "hexadecimal"

displayByteArrayAccess :: ByteArrayAccess ba => ba -> String
displayByteArrayAccess = display . fromBytesUnsafe . convertToBase Base16
{-# INLINABLE displayByteArrayAccess #-}
