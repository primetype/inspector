{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Inspector.Export.Types
    ( Export (..)
    , Description(..)
    , OutputType(..)
    , Inspectable(..)
    , ExportType(..)
    , TypeSize(..)
    , IntermediarType(..)
    , builderToString
    , input
    , output

    , withBoolean
    , withInteger
    , withDouble
    , withString
    , withCollection
    , withStructure
    ) where

import Foundation
import Foundation.String (fromBytesUnsafe)
import Foundation.String.Builder

import Basement.Block (Block)
import Basement.Nat
import Data.ByteArray (Bytes, convert)
import Data.ByteArray.Encoding

import Crypto.Hash (Digest, digestFromByteString)
import Crypto.Hash.IO (HashAlgorithm(..))
import Crypto.MAC.HMAC (HMAC(..))

import Control.Applicative (Alternative(..))
import Control.Monad (mapM)
import Data.Typeable
import GHC.ST (runST)

builderToString :: Builder -> String
builderToString a = runST (runUnsafe a)

instance Alternative (Either String) where
    empty = undefined
    (<|>) (Left _) r = r
    (<|>) l        _ = l

data OutputType
    = TestVector
    | Markdown
    | Rust
  deriving (Show, Eq, Ord, Enum, Bounded, Typeable)

data TypeSize = Size8 | Size16 | Size32 | Size64 | Size128 | Size256
  deriving (Show, Eq, Ord, Enum, Bounded, Typeable)

data E
  deriving (Typeable)

data ExportType
    = TypeBoolean
    | TypeSignedInteger !TypeSize
    | TypeUnsignedInteger !TypeSize
    | TypeDouble
    | TypeString
    | TypeArray !ExportType (Maybe (CountOf E))
    | TypeStruct ![(String, ExportType)]
  deriving (Show, Eq, Ord, Typeable)

data IntermediarType
    = ITBoolean    Bool
    | ITInteger    Integer
    | ITDouble     Double
    | ITString     String
    | ITCollection [IntermediarType]
    | ITStructure  [(String, IntermediarType)]
  deriving (Show, Eq, Ord, Typeable)

reportError :: String -> IntermediarType -> Either String a
reportError desc it = Left $ case it of
    ITBoolean    b   -> mkError $ "Received a boolean of value " <> show b
    ITInteger    i   -> mkError $ "Received an integer of value " <> show i
    ITDouble     d   -> mkError $ "Received an double of value " <> show d
    ITString     str -> mkError $ "Received a string of value " <> show str
    ITCollection col -> mkError $ "Received a collection of " <> show col
    ITStructure  str -> mkError $ "Received a strucuture " <> show str
  where
    mkError :: String -> String
    mkError more = "Error when parsing " <> desc <> ". " <> more

withBoolean :: String -> (Bool -> Either String a) -> IntermediarType -> Either String a
withBoolean _ f (ITBoolean b) = f b
withBoolean t _ r             = reportError t r

withInteger :: String -> (Integer -> Either String a) -> IntermediarType -> Either String a
withInteger _ f (ITInteger i) = f i
withInteger t _ r             = reportError t r

withDouble :: String -> (Double -> Either String a) -> IntermediarType -> Either String a
withDouble _ f (ITDouble i) = f i
withDouble t _ r             = reportError t r

withString :: String -> (String -> Either String a) -> IntermediarType -> Either String a
withString _ f (ITString str) = f str
withString t _ r             = reportError t r

withCollection :: String -> ([IntermediarType] -> Either String a) -> IntermediarType -> Either String a
withCollection _ f (ITCollection its) = f its
withCollection t _ r             = reportError t r

withStructure :: String -> ([(String, IntermediarType)] -> Either String a) -> IntermediarType -> Either String a
withStructure _ f (ITStructure nits) = f nits
withStructure t _ r             = reportError t r

class Inspectable a where
    -- | some documentation regarding the serialised format
    --
    -- This does not document the type itself, just the format.
    --
    documentation :: Proxy a -> String

    -- | the type of the serialised object compatible for the given `OutputType`
    exportType :: Proxy a -> ExportType

    parser :: IntermediarType -> Either String a

    -- | display the given object into a compatible format for the `OutputType`
    builder :: a -> IntermediarType

instance Inspectable Bool where
    documentation _ = "Boolean value, either true or false."
    exportType _ = TypeBoolean
    parser = withBoolean "Bool" pure
    builder = ITBoolean
instance Inspectable Int8 where
    documentation _ = "8 bits signed integer."
    exportType _ = TypeSignedInteger Size8
    parser = withInteger "Int8" (pure . fromInteger)
    builder  = ITInteger . toInteger
instance Inspectable Int16 where
    documentation _ = "16 bits signed integer."
    exportType _ = TypeSignedInteger Size16
    parser = withInteger "Int16" (pure . fromInteger)
    builder  = ITInteger . toInteger
instance Inspectable Int32 where
    documentation _ = "32 bits signed integer."
    exportType _ = TypeSignedInteger Size32
    parser = withInteger "Int32" (pure . fromInteger)
    builder  = ITInteger . toInteger
instance Inspectable Int64 where
    documentation _ = "64 bits signed integer."
    exportType _ = TypeSignedInteger Size64
    parser = withInteger "Int64" (pure . fromInteger)
    builder  = ITInteger . toInteger
instance Inspectable Int where
    documentation _ = "signed integer."
    exportType _ = TypeSignedInteger Size64
    parser = withInteger "Int" (pure . fromInteger)
    builder  = ITInteger . toInteger
instance Inspectable Integer where
    documentation _ = "signed unbounded integer"
    exportType _ = TypeSignedInteger Size64
    parser = withInteger "Integer" (pure . fromInteger)
    builder  = ITInteger
instance Inspectable Word8 where
    documentation _ = "8 bits unsigned integer."
    exportType _ = TypeUnsignedInteger Size8
    parser = withInteger "Word8" (pure . fromInteger)
    builder  = ITInteger . toInteger
instance Inspectable Word16 where
    documentation _ = "16 bits unsigned integer."
    exportType _ = TypeUnsignedInteger Size16
    parser = withInteger "Word16" (pure . fromInteger)
    builder  = ITInteger . toInteger
instance Inspectable Word32 where
    documentation _ = "32 bits unsigned integer."
    exportType _ = TypeUnsignedInteger Size32
    parser = withInteger "Word32" (pure . fromInteger)
    builder  = ITInteger . toInteger
instance Inspectable Word64 where
    documentation _ = "64 bits unsigned integer."
    exportType _ = TypeUnsignedInteger Size64
    parser = withInteger "Word64" (pure . fromInteger)
    builder  = ITInteger . toInteger
instance Inspectable Word where
    documentation _ = "unsigned integer."
    exportType _ = TypeUnsignedInteger Size64
    parser = withInteger "Word" (pure . fromInteger)
    builder  = ITInteger . toInteger
instance Inspectable Double where
    documentation _ = "64 bits float."
    exportType _ = TypeDouble
    parser = withDouble "Double" pure
    builder  = ITDouble
instance Inspectable String where
    documentation _ = "Bouble quoted, encoded string."
    exportType _ = TypeString
    parser = withString "String" pure
    builder  = ITString
instance Inspectable (Block Word8) where
    documentation _ = "hexadecimal encoded bytes"
    exportType _ = TypeArray (TypeUnsignedInteger Size8) Nothing
    parser it = withCollection "Block Word8" (fmap fromList . mapM parser) it
            <|> withString "Block Word8" (first fromList . convertFromBase Base16) it
    -- builder blck = ITCollection $ builder <$> toList blck
    builder = ITString . fromBytesUnsafe . convertToBase Base16
instance (Typeable a, Inspectable a) => Inspectable [a] where
    documentation _ = "collection of " <> documentation (Proxy @a)
    exportType _ = TypeArray (exportType (Proxy @a)) Nothing
    parser = withCollection ("["<>show (typeRep (Proxy @a))<>"]") $ fmap fromList . mapM parser
    builder l = ITCollection $ builder <$> l
instance (HashAlgorithm hash, KnownNat (HashDigestSize hash)) => Inspectable (Digest hash) where
    documentation _ = "hexadecimal encoded bytes"
    exportType _ = TypeArray (TypeSignedInteger Size8) (Just size)
      where
        size = fromInteger $ natVal $ Proxy @(HashDigestSize hash)
    parser t = do
        blk <- parser t :: Either String (Block Word8)
        case digestFromByteString blk of
            Nothing -> reportError "invalid digest" t
            Just v  -> pure v
    builder t = builder (convert t :: Block Word8)
instance Inspectable Bytes where
    documentation _ = "hexadecimal encoded bytes"
    exportType _ = exportType (Proxy @(Block Word8))
    parser t = convert <$> (parser t :: Either String (Block Word8))
    builder t = builder (convert t :: Block Word8)
instance (HashAlgorithm hash, KnownNat (HashDigestSize hash)) => Inspectable (HMAC hash) where
    documentation _ = "hexadecimal encoded bytes"
    exportType _ = exportType (Proxy @(Digest hash))
    parser t = HMAC <$> parser t
    builder t = builder (convert t :: Block Word8)

data Description = Description
    { descriptionKey      :: !String
    , descriptionEncoding :: !String
    , descriptionType     :: !TypeRep
    , descriptionTargetType :: !ExportType
    , descriptionComment  :: !(Maybe String)
    }
  deriving (Show, Typeable)

input :: Description -> Export
input i = Export [i] mempty

output :: Description -> Export
output o = Export mempty [o]

data Export = Export
    { exportInputs :: ![Description]
    , exportOutpus :: ![Description]
    }
  deriving (Typeable)
instance Semigroup Export where
    (<>) = mappend
instance Monoid Export where
    mempty = Export mempty mempty
    mappend (Export a b) (Export x y) =
        Export (a <> x) (b <> y)
