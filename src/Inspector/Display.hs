
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

instance Display Bool where
    display True = "true"
    display False = "false"
instance Display Int where
    display = show
instance Display Int16 where
    display = show
instance Display Int32 where
    display = show
instance Display Int64 where
    display = show
instance Display Word where
    display = show
instance Display Word16 where
    display = show
instance Display Word32 where
    display = show
instance Display Word64 where
    display = show
instance Display Integer where
    display = show
instance Display Natural where
    display = show
instance Display Double where
    display = show
instance Display String where
    display str = "\"" <> str <> "\""
instance Display Bytes where
    display = displayByteArrayAccess
instance Display ScrubbedBytes where
    display = displayByteArrayAccess
instance PrimType ty => Display (UArray ty) where
    display = displayByteArrayAccess
instance HashAlgorithm hash => Display (Digest hash) where
    display = displayByteArrayAccess
instance HashAlgorithm hash => Display (HMAC hash) where
    display = displayByteArrayAccess . hmacGetDigest
instance (Display a, Display b) => Display (a,b) where
    display (a,b) = "(" <> display a <> ", " <> display b <> ")"
instance (Display a, Display b, Display c) => Display (a,b,c) where
    display (a,b,c) = "(" <> display a <> ", " <> display b <> ", " <> display c <> ")"
instance (Display a, Display b, Display c, Display d) => Display (a,b,c,d) where
    display (a,b,c,d) = "(" <> display a <> ", " <> display b <> ", " <> display c <> ", " <> display d <> ")"

displayByteArrayAccess :: ByteArrayAccess ba => ba -> String
displayByteArrayAccess = display . fromBytesUnsafe . convertToBase Base16
{-# INLINABLE displayByteArrayAccess #-}
