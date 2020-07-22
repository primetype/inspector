{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inspector.Builder
    ( Builder
    , runBuilder

    , newline
    , emit
    , indent
    , unindent
    ) where

import Foundation
import Foundation.Monad
import Foundation.Monad.State

import qualified Foundation.String.Builder as Builder

import Control.Monad (when)

newtype Indentation = Indentation [String]
  deriving (Show, Eq, Semigroup, Monoid)
instance IsList Indentation where
    type Item Indentation = String
    toList (Indentation l) = l
    fromList = Indentation

getIndent :: Indentation -> String
getIndent (Indentation []) = mempty
getIndent (Indentation (x:xs)) = x <> getIndent (Indentation xs)

popIndent :: Indentation -> Indentation
popIndent (Indentation []) = Indentation []
popIndent (Indentation (_:xs)) = Indentation xs

pushIndent :: Indentation -> String -> Indentation
pushIndent (Indentation xs) x = Indentation (x:xs)

data BuilderState = BuilderState
    { currentIndentation :: Indentation
    , currentBuilder     :: Builder.Builder
    , currentStatus      :: BuilderStatus
    }
data BuilderStatus = BSNone | BSStarted
  deriving (Show, Eq, Ord, Enum, Bounded)

defaultState :: BuilderState
defaultState = BuilderState
    { currentIndentation = mempty
    , currentBuilder     = mempty
    , currentStatus      = BSNone
    }

newtype Builder a = Builder { runBuilder_ :: StateT BuilderState Identity a }
  deriving (Functor, Applicative, Monad)

runBuilder :: Builder a -> String
runBuilder builder =
    let Identity (_, st) = runStateT (runBuilder_ builder) defaultState
     in Builder.toString (currentBuilder st)

asks :: (BuilderState -> b) -> Builder b
asks f = Builder $ withState $ \st -> (f st, st)

update :: (BuilderState -> BuilderState) -> Builder ()
update f = Builder $ withState $ \st -> ((), f st)

appendBuilder :: Builder.Builder -> Builder ()
appendBuilder builder = update $ \st -> st { currentBuilder = currentBuilder st <> builder }

newline :: Builder ()
newline = do
    appendBuilder $ Builder.emitChar '\n'
    update $ \st -> st { currentStatus = BSNone }

indent :: CountOf Char -> Builder ()
indent n = update $ \st -> st
    { currentIndentation = pushIndent (currentIndentation st) (replicate n ' ')
    }

unindent :: Builder ()
unindent = update $ \st -> st
    { currentIndentation = popIndent (currentIndentation st) }

emitIndent :: Builder ()
emitIndent = do
    i <- asks $ getIndent . currentIndentation
    appendBuilder $ Builder.emit i
    update $ \st -> st { currentStatus = BSStarted }

emit :: String -> Builder ()
emit str = do
    st <- asks currentStatus
    when (st == BSNone) emitIndent
    appendBuilder (Builder.emit str)