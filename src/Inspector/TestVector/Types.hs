{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Inspector.TestVector.Types
    ( Type(..)
    , Array(..)
    , Object(..)
    , isByteArray
    , innerType
    ) where

import Foundation hiding (String, Array)
import Foundation.Collection (KeyedCollection, Element, IndexedCollection(..))
import qualified Foundation.Collection as F

import Inspector.TestVector.Key (Key)

data Type
    = Boolean
    | Unsigned8
    | Unsigned16
    | Unsigned32
    | Unsigned64
    | Signed8
    | Signed16
    | Signed32
    | Signed64
    | Float32
    | Float64
    | String
    | Array !Array
    | Object !Object
  deriving (Show, Eq, Ord, Typeable)

isByteArray :: Type -> Bool
isByteArray (Array (SizedArray Unsigned8 _)) = True
isByteArray (Array (UnsizedArray Unsigned8)) = True
isByteArray _                                = False

innerType :: Type -> Type
innerType (Array (SizedArray t _)) = t
innerType (Array (UnsizedArray t)) = t
innerType _ = undefined

data Array
    = SizedArray   !Type !Word64
    | UnsizedArray !Type
  deriving (Show, Eq, Ord, Typeable)

newtype Object = ObjectDef [(Key, Type)]
  deriving (Show, Eq, Ord, Typeable, Semigroup, Monoid, Collection, Sequential, IndexedCollection, Foldable)

type instance Element Object = (Key, Type)

instance KeyedCollection Object where
    type Key Object   = Key
    type Value Object = Type
    lookup k = F.lookup k . toList

instance IsList Object where
    type Item Object = (Key, Type)
    toList (ObjectDef l) = l
    fromList = ObjectDef
