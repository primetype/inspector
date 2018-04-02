{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Inspector.TestVector.Key
    ( Key
    , keyToString
    , mkKey
    , symbolKey
    , symbolKey_
    , keyParser
    ) where

import           Foundation
import           Foundation.Check (Arbitrary(..), between, frequency, elements)
import           Foundation.Collection (nonEmpty_)

import           Basement.NormalForm ()
import           GHC.TypeLits
import           Data.Typeable (typeRep)

import           Control.Monad (replicateM)

import           Inspector.Parser (Parser)
import qualified Inspector.Parser as Parser

-- | Key
--
-- Valid key are of the form: `[a-z][a-zA-Z0-9_]*`
--
newtype Key = Key { keyToString :: String }
  deriving (Show, Eq, Ord, Typeable, NormalForm)
instance IsString Key where
    fromString str = maybe (error $ "invalid Key... " <> show str) id $ mkKey $ fromList str

instance Arbitrary Key where
    arbitrary = do
        x <- elements $ nonEmpty_ ['a'..'z']
        len <- fromIntegral <$> between (0, 7)
        xs <- replicateM len validCharGen
        pure $ Key . fromList $ x : xs
      where
        validCharGen = frequency $ nonEmpty_
            [ (65, elements $ nonEmpty_ ['a'..'z'])
            , (20, elements $ nonEmpty_ ['A'..'Z'])
            , (10, elements $ nonEmpty_ ['0'..'9'])
            , ( 5, pure '_')
            ]

symbolKey_ :: forall key. KnownSymbol key => Proxy key -> Key
symbolKey_ proxy = fromMaybe errorMsg $ symbolKey proxy
  where
    errorMsg = error $ "Invalid Symbol Key <> " <> show (typeRep proxy)

symbolKey :: forall key. KnownSymbol key => Proxy key -> Maybe Key
symbolKey proxy = mkKey str
  where
    str = fromList $ symbolVal proxy

-- | smart constructor, check the key is valid before constructing it
--
-- this function use `keyParser`
mkKey :: String -> Maybe Key
mkKey str = case Parser.parseOnly keyParser str of
    Left _ -> Nothing
    Right (k, _) -> Just k

-- | key parser
--
keyParser :: Parser Key
keyParser = do
    x <- Parser.satisfy (`elem` validStartChar)
    xs <- Parser.takeWhile_ (`elem` validChar)
    pure $ Key $ cons x xs

validStartChar :: [Char]
validStartChar = ['a'..'z']

validChar :: [Char]
validChar = validStartChar <> ['A'..'Z'] <> ['0'..'9'] <> ['_']
