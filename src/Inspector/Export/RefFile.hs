
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inspector.Export.RefFile
    ( RefFile(..)
    , Dict
    , parseRefFile
    , writeRefFile

    , storeBackDictC
    ) where

import Foundation
import Foundation.IO
import Foundation.Conduit
import Foundation.Conduit.Textual
import Foundation.Monad
import Foundation.Monad.State
import Foundation.String (Encoding(UTF8))
import Foundation.String.Read

import Foundation.VFS.FilePath
import           Foundation.Parser (ParseError(..), Result(..))
import qualified Foundation.Parser as Parser

import Control.Applicative (Alternative(..))

import Inspector.Export.Types
import Inspector.Dict

import Data.List (zip)
import Control.Monad (forM_)

newtype RefFile = RefFile [Dict]
  deriving (Eq, Typeable, Show)
instance IsList RefFile where
    type Item RefFile = Dict
    toList (RefFile l) = l
    fromList = RefFile

writeRefFile :: FilePath -> RefFile -> IO ()
writeRefFile fp (RefFile dics) = withFile fp WriteMode $ \h -> runConduit $
    yields (zip [1..] dics) .| storeBackDictC .| toBytes UTF8 .| sinkHandle h

storeBackDictC :: Monad m => Conduit (Word, Dict) String m ()
storeBackDictC = dictC
  where
    dictC :: Monad m => Conduit (Word,Dict) String m ()
    dictC = awaitForever $ \(w, l) -> do
        yield $ "# Test Vector " <> show w <> "\n"
        yield "TestVector\n"
        forM_ (sortBy f $ toList l) $ \(k, it) -> do
          yield k
          yield " = "
          yield (buildIntermediarType (replicate (length k + 3) ' ') it)
          yield "\n"
        yield "\n"
      where
        f (x,_) (y,_) = compare x y
    buildIntermediarType :: String -> IntermediarType -> String
    buildIntermediarType alignment it = case it of
        ITBoolean    b   -> if b then "true" else "false"
        ITInteger    i   -> show i
        ITDouble     d   -> show d
        ITString     str -> show str
        ITCollection [] -> "[]"
        ITCollection [x] -> "[ " <> buildIntermediarType alignment x <> " ]"
        ITCollection xs  -> "[ "
                         <> intercalate ("\n"<> alignment <> ", ")
                                        (buildIntermediarType (alignment <> "  ") <$> xs)
                         <> "\n" <> alignment <> "]"
        ITStructure  [] -> "{}"
        ITStructure  [(n,v)] -> "{ " <> n <> " = " <> buildIntermediarType (alignment <> replicate (length n) ' ' <> "     ") v <> " }"
        ITStructure  str -> "{ "
                         <> intercalate ("\n"<> alignment <> ", ")
                                        ((\(n, v) -> n <> " = " <> buildIntermediarType (alignment <> replicate (length n) ' ' <> "     ") v) <$> str)
                         <> "\n" <> alignment <> "}"

parseRefFile :: FilePath -> IO RefFile
parseRefFile fp = withFile fp ReadMode $ \h -> runConduit $
    sourceHandle h .| fromBytes UTF8 .| parseC .| RefFile <$> sinkList

parseC :: Monad m => Conduit String Dict m ()
parseC = go (S 0)
  where
    go st = do
        mstr <- await
        case mstr of
            Nothing -> pure ()
            Just str ->
                case runParser st (Parser.optional parseDict) str of
                    ParseOk str' (Just r, st') -> leftover str' >> yield r >> go st'
                    ParseOk str' (Nothing, st') -> error $ show ("not enought (parseDict)" :: String, str', st')
                    ParseFailed err         -> error $ show err
                    ParseMore   more        -> go' more
    go' more = do
        mstr <- await
        case mstr of
            Nothing -> case more mempty of
                ParseOk str' (Just r, st) -> leftover str' >> yield r >> go st
                ParseOk _ (Nothing, _) -> pure () -- error $ show ("not enought (more empty)", str', st)
                ParseFailed err  -> error $ show err
                ParseMore   _ -> error "ParserMore (more mempty)"
            Just str -> case more str of
                ParseOk str' (Just r, st) -> leftover str' >> yield r >> go st
                ParseOk str' (Nothing, st) -> error $ show ("not enought (more str)" :: String, str, str', st)
                ParseFailed err  -> error $ show err
                ParseMore   more' -> go' more'


parseDict :: Parser Dict
parseDict = fmap fromList $ do
    _ <- many comment
    elements "TestVector"
    newline
    some go
  where
    go = do
        whiteSpacesAndNewLines
        r <- parserEntry <* newline
        whiteSpacesAndNewLines
        pure r

parserEntry :: Parser (String, IntermediarType)
parserEntry = do
    name <- parserFieldName
    whiteSpaces
    element '='
    whiteSpaces
    (name, ) <$> parserIntermediarType

newtype S = S { lineParsed :: Word }
  deriving (Typeable, Show, Eq, Ord)

runParser :: S -> Parser a -> String -> Result String (a, S)
runParser st (Parser s) = Parser.parse (runStateT s st)

newtype Parser a = Parser (StateT S (Parser.Parser String) a)
  deriving (Functor, Applicative, Monad)
instance Alternative Parser where
    empty = Parser $ lift empty
    (<|>) (Parser s1) (Parser s2) = Parser $ do
        st <- withState $ \st -> (st, st)
        r <- lift $ runStateT s1 st <|> runStateT s2 st
        withState $ const r

elements :: String -> Parser ()
elements = Parser . lift . Parser.elements

element :: Char -> Parser ()
element = Parser . lift . Parser.element

skip :: CountOf Char -> Parser ()
skip = Parser . lift . Parser.skip

takeWhile_ :: (Char -> Bool) -> Parser String
takeWhile_ = Parser . lift . Parser.takeWhile

whiteSpaces, whiteSpacesAndNewLines :: Parser ()
whiteSpaces = many whiteSpace >> pure ()
whiteSpacesAndNewLines = many (whiteSpace <|> newline) >> pure ()

whiteSpace :: Parser ()
whiteSpace = element ' ' <|> element '\t'

newline :: Parser ()
newline = do
    element '\n'
    Parser $ withState $ \st -> ((), st { lineParsed = succ (lineParsed st) })

comment :: Parser ()
comment = element '#' *> takeWhile_ ('\n' /=) *> newline

reportError :: ParseError String -> Parser a
reportError err = do
    _lp <- Parser $ withState $ \st -> (lineParsed st, st)
    Parser $ lift $ Parser.reportError err

parserIntermediarType :: Parser IntermediarType
parserIntermediarType = parserBool
                    <|> parserDouble
                    <|> parserInteger
                    <|> parserString
                    <|> parserCollection
                    <|> parserStructure

parserBool :: Parser IntermediarType
parserBool = fmap ITBoolean $ (elements "true" >> pure True)
                          <|> (elements "false" >> pure False)
                          <|> reportError (Expected "true or false" "")

parserInteger :: Parser IntermediarType
parserInteger = fmap ITInteger $ do
    r <- takeWhile_ (`elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
    case readInteger r of
        Nothing -> reportError (Expected "Integer" ("got " <> r))
        Just v  -> pure v

parserDouble :: Parser IntermediarType
parserDouble = fmap ITDouble $ do
    r <- takeWhile_ (`elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
    element '.'
    d <- takeWhile_ (`elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
    mexp <- Parser.optional $ do
                element 'e'
                ("e" <>) <$> takeWhile_ (`elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-'])
    case readDouble (r <> "." <> d <> maybe mempty id mexp) of
        Nothing -> reportError (Expected "Double" ("received invalid: " <> r))
        Just v  -> pure v

parserString :: Parser IntermediarType
parserString = fmap ITString $ element '"' *> quotedParser <* element '"'
  where
    quotedParser = do
        s <- takeWhile_ ('"' /=)
        case unsnoc s of
            Just (_, '\\') -> skip 1 >> mappend (snoc s '"') <$> quotedParser
            _              -> pure s

parserCollection :: Parser IntermediarType
parserCollection = fmap ITCollection $ do
    element '['
    whiteSpacesAndNewLines
    l <- go <|> pure []
    whiteSpacesAndNewLines
    element ']'
    pure l
  where
    go = do
        whiteSpacesAndNewLines
        r <- parserIntermediarType
        whiteSpacesAndNewLines
        (element ',' *> ((:) r <$> go)) <|> pure [r]


parserFieldName :: Parser String
parserFieldName = takeWhile_ (`elem` (['a'..'z'] <> ['A'..'Z'] <> ['_']))

parserStructure :: Parser IntermediarType
parserStructure = fmap ITStructure $ do
    element '{'
    whiteSpacesAndNewLines
    l <- go <|> pure []
    whiteSpacesAndNewLines
    element '}'
    pure l
  where
    go = do
        whiteSpacesAndNewLines
        n <- parserFieldName
        whiteSpacesAndNewLines
        element '='
        whiteSpacesAndNewLines
        r <- parserIntermediarType
        whiteSpacesAndNewLines
        (element ',' *> ((:) (n, r) <$> go)) <|> pure [(n, r)]
