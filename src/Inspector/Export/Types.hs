{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Inspector.Export.Types
    ( Export (..)
    , Description(..)
    , OutputType(..)
    , Inspectable(..)
    , builderToString
    , input
    , output
    ) where

import Foundation
import Foundation.Monad
import Foundation.Parser
import qualified Foundation.Parser as Parser
import Foundation.String (replace)
import Foundation.String.Builder
import qualified Basement.Block.Builder as B
import Foundation.Primitive
import Foundation.String.Read

import Basement.Block (Block)
import Data.ByteArray (Bytes, convert)
import Data.ByteArray.Encoding

import Crypto.Hash (Digest, digestFromByteString, HashAlgorithm)
import Crypto.MAC.HMAC (HMAC(..))

import Data.Typeable
import GHC.ST (runST)

builderToString :: Builder -> String
builderToString a = runST (runUnsafe a)

data OutputType
    = TestVector
    | Markdown
    | Rust
  deriving (Show, Eq, Ord, Enum, Bounded, Typeable)

class Inspectable a where
    -- | some documentation regarding the serialised format
    --
    -- This does not document the type itself, just the format.
    --
    documentation :: Proxy a -> String

    -- | the type of the serialised object compatible for the given `OutputType`
    exportType :: Proxy a -> OutputType -> Builder

    -- | parser for the 'TestVector' type.
    --
    -- > parser . display TestVector == id
    --
    parser  :: Proxy a -> Parser String a

    -- | display the given object into a compatible format for the `OutputType`
    display :: OutputType -> a -> Builder

instance Inspectable Bool where
    documentation _ = "Boolean value, either true or false."
    exportType _ Rust       = emit "bool"
    exportType _ _          = emit "Boolean"
    parser _ = (elements "true"  >> pure True)
           <|> (elements "false" >> pure False)
           <|> (takeAll >>= reportError . Expected "true or false")
    display _    True = emit "true"
    display _   False = emit "false"
instance Inspectable Int8 where
    documentation _ = "8 bits signed integer."
    exportType _ Rust       = emit "i8"
    exportType _ _          = emit "Int8"
    parser _ = fromIntegral <$> (parseIntegral :: Parser String Int16)
    display _ = emit . show
instance Inspectable Int16 where
    documentation _ = "16 bits signed integer."
    exportType _ Rust       = emit "i16"
    exportType _ _          = emit "Int16"
    parser _ = parseIntegral
    display _ = emit . show
instance Inspectable Int32 where
    documentation _ = "32 bits signed integer."
    exportType _ Rust       = emit "i32"
    exportType _ _          = emit "Int32"
    parser _ = parseIntegral
    display _ = emit . show
instance Inspectable Int64 where
    documentation _ = "64 bits signed integer."
    exportType _ Rust       = emit "i64"
    exportType _ _          = emit "Int64"
    parser _ = parseIntegral
    display _ = emit . show
instance Inspectable Int where
    documentation _ = "signed integer."
    exportType _ Rust       = emit "i64"
    exportType _ _          = emit "Int"
    parser _ = parseIntegral
    display _ = emit . show
instance Inspectable Word8 where
    documentation _ = "8 bits unsigned integer."
    exportType _ Rust       = emit "u8"
    exportType _ _          = emit "Word8"
    parser _ = fromIntegral <$> (parseIntegral :: Parser String Word16)
    display _ = emit . show
instance Inspectable Word16 where
    documentation _ = "16 bits unsigned integer."
    exportType _ Rust       = emit "u16"
    exportType _ _          = emit "Word16"
    parser _ = parseIntegral
    display _ = emit . show
instance Inspectable Word32 where
    documentation _ = "32 bits unsigned integer."
    exportType _ Rust       = emit "u32"
    exportType _ _          = emit "Word32"
    parser _ = parseIntegral
    display _ = emit . show
instance Inspectable Word64 where
    documentation _ = "64 bits unsigned integer."
    exportType _ Rust       = emit "u64"
    exportType _ _          = emit "Word64"
    parser _ = parseIntegral
    display _ = emit . show
instance Inspectable Word where
    documentation _ = "unsigned integer."
    exportType _ Rust       = emit "u64"
    exportType _ _          = emit "Word"
    parser _ = parseIntegral
    display _ = emit . show
instance Inspectable Double where
    documentation _ = "64 bits float."
    exportType _ Rust       = emit "f64"
    exportType _ _          = emit "Double"
    parser _ = do
        r <- Parser.takeWhile (`elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.'])
        maybe (reportError (Expected "Double" ("received invalid: " <> r)))
              pure
              (readDouble r)
    display _ = emit . show
instance Inspectable String where
    documentation _ = "Bouble quoted, encoded string."
    exportType _ _ = emit "String"
    parser _ = element '"' *> quotedParser <* element '"'
      where
        quotedParser = do
            s <- Parser.takeWhile ('"' /=)
            case unsnoc s of
                Just (_, '\\') -> skip 1 >> mappend (snoc s '"') <$> quotedParser
                _              -> pure s
            pure s
    display _ s = emitChar '"' <> emit (replace "\"" "\\\"" s) <> emitChar '"'
instance Inspectable (Block Word8) where
    documentation _ = "hexadecimal encoded bytes"
    exportType _ = exportType (Proxy @String)
    parser _ = do
        hex <- (element '"' *> Parser.takeWhile isHexa <* element '"') <?> (Satisfy $ Just "hexadecimal characters")
        case convertFromBase Base16 hex of
            Left err -> reportError $ Expected "hexadecimal encoded bytes" (fromList err)
            Right v  -> pure v
      where
        isHexa = flip elem ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']
    display t = display t . builderToString . unsafeStringBuilder . B.emit . (convertToBase Base16 :: Block Word8 -> Block Word8)
instance Inspectable a => Inspectable [a] where
    documentation _ = "collection of " <> documentation (Proxy @a)
    exportType p t = emitChar '[' <> exportType p t <> emitChar ']'
    parser _ = do
        element '['
        l <- go <|> pure []
        element ']'
        pure l
      where
        go = do
            skipWhile (`elem` [' ', '\t'])
            r <- parser (Proxy @a)
            skipWhile (`elem` [' ', '\t'])
            (element ',' *> ((:) r <$> go)) <|> pure [r]
    display t l =  emitChar '['
                <> intercalate (emit ", ") (display t <$> l)
                <> emitChar ']'
instance HashAlgorithm hash => Inspectable (Digest hash) where
    documentation _ = "hexadecimal encoded bytes"
    exportType _ = exportType (Proxy @String)
    parser _ = do
        b <- parser (Proxy @(Block Word8))
        case digestFromByteString b of
            Nothing -> reportError (Expected "Digest" "invalid digest")
            Just d  -> pure d
    display t = display t . (convert :: Digest hash -> Block Word8)
instance Inspectable Bytes where
    documentation _ = "hexadecimal encoded bytes"
    exportType _ = exportType (Proxy @String)
    parser _ = convert <$> parser (Proxy @(Block Word8))
    display t = display t . (convert :: Bytes -> Block Word8)
instance HashAlgorithm hash => Inspectable (HMAC hash) where
    documentation _ = "hexadecimal encoded bytes"
    exportType _ = exportType (Proxy @String)
    parser _ = HMAC <$> parser Proxy
    display t = display t . (convert :: HMAC hash -> Block Word8)

(<?>) :: ParserSource s => Parser s a -> ParseError s -> Parser s a
(<?>) p err = p <|> reportError err
infixl 9 <?>

parseIntegral :: (HasNegation i, IntegralUpsize Word8 i, Additive i, Multiplicative i, IsIntegral i)
              => Parser String i
parseIntegral = do
    r <- Parser.takeWhile (`elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
    case readIntegral r of
        Nothing -> reportError (Expected "Integral type" ("got " <> r))
        Just v  -> pure v

data Description = Description
    { descriptionKey      :: !String
    , descriptionEncoding :: !String
    , descriptionType     :: !TypeRep
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
instance Semigroup Export
instance Monoid Export where
    mempty = Export mempty mempty
    mappend (Export a b) (Export x y) =
        Export (a <> x) (b <> y)
