{-# LANGUAGE FlexibleContexts #-}

module Inspector.Parser
    ( HasParser (..)
    , ParseError (..)
    , Parser

    , parseOnly
    , parseByteArray
    , strParser
    , reportError
    ) where

import Foundation hiding (takeWhile)
import Foundation.Parser
import Foundation.Primitive
import Foundation.String.Read

import Data.ByteArray
import Data.ByteArray.Encoding

import Crypto.Hash (Digest, digestFromByteString, HashAlgorithm)
import Crypto.MAC.HMAC (HMAC(..))

class HasParser a where
    getParser :: Parser String a

instance HasParser Bool where
    getParser = (elements "true" >> pure True) <|> (elements "false" >> pure False)
instance HasParser Integer where
    getParser = do
        r <- takeWhile (`elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
        case readInteger r of
            Nothing -> reportError (Expected "Integer" "invalid integer")
            Just i  -> pure i
instance HasParser Natural where
    getParser = do
        r <- takeWhile (`elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
        case readNatural r of
            Nothing -> reportError (Expected "Natural" "invalid natural")
            Just i  -> pure i
instance HasParser Int where
    getParser = parseIntegral
instance HasParser Int16 where
    getParser = parseIntegral
instance HasParser Int32 where
    getParser = parseIntegral
instance HasParser Int64 where
    getParser = parseIntegral
instance HasParser Word where
    getParser = parseIntegral
instance HasParser Word16 where
    getParser = parseIntegral
instance HasParser Word32 where
    getParser = parseIntegral
instance HasParser Word64 where
    getParser = parseIntegral
instance HasParser Double where
    getParser = do
        r <- takeWhile (`elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.'])
        case readDouble r of
            Nothing -> reportError (Expected "Double" ("received invalid: " <> r))
            Just v  -> pure v
instance HasParser String where
    getParser = strParser
instance HasParser Bytes where
    getParser = strParser >>= parseByteArray
instance HasParser ScrubbedBytes where
    getParser = strParser >>= parseByteArray
instance (Ord ty, PrimType ty) => HasParser (UArray ty) where
    getParser = strParser >>= parseByteArray
instance HashAlgorithm a => HasParser (Digest a) where
    getParser = do
        r <- strParser >>= parseByteArray
        case digestFromByteString (r :: Bytes) of
            Nothing -> reportError (Expected "Digest" "invalid digest")
            Just d  -> pure d
instance HashAlgorithm a => HasParser (HMAC a) where
    getParser = HMAC <$> getParser

instance HasParser a => HasParser [a] where
    getParser = do
        element '['
        l <- go <|> pure []
        element ']'
        pure l
      where
        go = do
            skipWhile (`elem` [' ', '\t'])
            r <- getParser
            skipWhile (`elem` [' ', '\t'])
            (element ',' *> ((:) r <$> go)) <|> pure [r]

parseByteArray :: (ByteArrayAccess ba, ByteArray a) => ba -> Parser String a
parseByteArray input = case convertFromBase Base16 input of
    Left err -> reportError (Expected "Base16" (fromList err))
    Right v  -> pure v

strParser :: Parser String String
strParser = element '"' *> takeWhile ('"' /=) <* element '"'

parseIntegral :: (HasNegation i, IntegralUpsize Word8 i, Additive i, Multiplicative i, IsIntegral i)
              => Parser String i
parseIntegral = do
    r <- takeWhile (`elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
    case readIntegral r of
        Nothing -> reportError (Expected "Integral type" ("got " <> r))
        Just v  -> pure v
