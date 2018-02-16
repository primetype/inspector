module Inspector.Export.Types
    ( Export (..)
    , Description(..)
    , input
    , output
    ) where

import Foundation
import Foundation.String
import Foundation.String.Builder

import Data.Typeable

data Description = Description
    { descriptionKey      :: !String
    , descriptionEncoding :: !String -- the format it is written in
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
