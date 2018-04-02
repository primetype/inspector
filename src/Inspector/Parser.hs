{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inspector.Parser
    ( Parser
    , S, lineParsed
    , reportError
    , ParseError(..)
    , Result(..)
    , parse
    , defaultS
    , parseOnly

    , Parser.optional
    , elements
    , element
    , skip
    , takeWhile_
    , satisfy
    , many
    , some

    , whiteSpace
    , newline
    , whiteSpaces
    , whiteSpacesAndNewLines
    , comment
    ) where

import           Foundation
import           Foundation.Parser (ParseError(..), Result(..))
import qualified Foundation.Parser as Parser
import           Foundation.Monad
import           Foundation.Monad.State

import           Control.Applicative (Alternative(..))

newtype S = S { lineParsed :: Word }
  deriving (Typeable, Show, Eq, Ord)

defaultS :: S
defaultS = S 0

parse :: S -> Parser a -> String -> Result String (a, S)
parse st (Parser s) = Parser.parse (runStateT s st)

parseOnly :: Parser a -> String -> Either (ParseError String) (a, S)
parseOnly (Parser s) = Parser.parseOnly (runStateT s defaultS)

newtype Parser a = Parser (StateT S (Parser.Parser String) a)
  deriving (Functor, Applicative, Monad)
instance Alternative Parser where
    empty = Parser $ lift empty
    (<|>) (Parser s1) (Parser s2) = Parser $ do
        st <- withState $ \st -> (st, st)
        r <- lift $ runStateT s1 st <|> runStateT s2 st
        withState $ const r

reportError :: ParseError String -> Parser a
reportError err = do
    _lp <- Parser $ withState $ \st -> (lineParsed st, st)
    Parser $ lift $ Parser.reportError err


elements :: String -> Parser ()
elements = Parser . lift . Parser.elements

element :: Char -> Parser ()
element = Parser . lift . Parser.element

skip :: CountOf Char -> Parser ()
skip = Parser . lift . Parser.skip

takeWhile_ :: (Char -> Bool) -> Parser String
takeWhile_ = Parser . lift . Parser.takeWhile

satisfy :: (Char -> Bool) -> Parser Char
satisfy = Parser . lift . Parser.satisfy_

whiteSpace :: Parser ()
whiteSpace = element ' ' <|> element '\t'

newline :: Parser ()
newline = do
    element '\n'
    Parser $ withState $ \st -> ((), st { lineParsed = succ (lineParsed st) })

comment :: Parser ()
comment = element '#' *> takeWhile_ ('\n' /=) *> newline

whiteSpaces, whiteSpacesAndNewLines :: Parser ()
whiteSpaces = many whiteSpace >> pure ()
whiteSpacesAndNewLines = many (whiteSpace <|> newline) >> pure ()
