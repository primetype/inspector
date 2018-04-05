{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inspector.Export.Rust
    ( run
    ) where

import Foundation
import Foundation.String (upper)
import Foundation.VFS.FilePath (FilePath)
import Foundation.Collection (nonEmpty_, KeyedCollection, IndexedCollection, Element)
import qualified Foundation.Collection as F
import Foundation.Monad
import Foundation.Monad.State
import Foundation.IO (withFile, IOMode(..), hPut)
import Foundation.VFS.FilePath
import Foundation.String (toBytes, Encoding(UTF8))

import Control.Monad (forM_, forM, when)

import Inspector.Monad (GoldenT, Config(..), ask, mkPath)
import Inspector.Builder
import Inspector.Export.Types (liftValue)
import Inspector.TestVector.TestVector (TestVector, Entry(..), inputs, outputs)
import Inspector.TestVector.Key (Key)
import Inspector.TestVector.Value (Value)
import qualified Inspector.TestVector.Value as Value
import Inspector.TestVector.Types (Type)
import qualified Inspector.TestVector.Types as Type
import Inspector.TestVector.Key (keyToString)

run :: FilePath -> [(Word, TestVector (Type, Value, Value))] -> GoldenT ()
run path tvs = do
    stdout <- getStdout <$> ask
    fp <- mkfp

    let out = runBuilder $ buildRust tvs

    liftIO $ if stdout
        then putStrLn out
        else withFile fp  WriteMode $ flip hPut (toBytes UTF8 out)
  where
    mkfp = do
        fp <- mkPath path
        pure $ fromString $ toList $ (filePathToString fp) <> ".rs"

buildRust :: [(Word, TestVector (Type, Value, Value))] -> Builder ()
buildRust tvs = do
    defineTestVector $ snd $ head $ nonEmpty_ tvs
    newline
    emit $ "const TEST_VECTORS : [TestVector;" <> show (fromCount (length tvs)) <> "] ="
    newline
    indent 4
    consumeTestVectors tvs
    unindent

consumeTestVectors :: [(Word, TestVector (Type, Value, Value))] -> Builder ()
consumeTestVectors l = emit "[ " >> go False l >> emit "];" >> newline
  where
    go _ [] = pure ()
    go b ((_, tv):xs) = do
        when b $ emit ", "
        emit "TestVector {" >> newline >> indent 4
        forM_ (inputs tv) go'
        forM_ (outputs tv) go'
        unindent >> emit "  }" >> newline
        go True xs
    go' ent = do
        let str = keyToString (entryKey ent)
        let (t, v, _) = entryExtra ent
        emit str >> emit ": " >> valueBuilder (entryKey ent) (liftValue t v) >> emit "," >> newline

defineTestVector :: TestVector (Type, Value, Value) -> Builder ()
defineTestVector tv = do
    let (tvtype, defs) = runConvertion $ convertTestVector tv

    forM_ (toList defs) preDef
    defineType tvtype
  where
    preDef (TypeName tn, ro) = do
        emit "#[derive(Debug)]" >> newline
        emit ("struct " <> tn <> " {") >> newline
        indent 4
        forM_ (toList ro) go
        unindent >> emit "}" >> newline
      where
        go (k, rt) = do
            let str = keyToString k <> ": "
            emit str
            indent (length str)
            case rt of
                CompatibleType t -> emitType t
                DefinedType (TypeName n) -> emit n
            emit ","
            unindent
            newline

defineType :: [(Key, Bool, Maybe String, RustType)] -> Builder ()
defineType l = do
    emit "#[derive(Debug)]" >> newline
    emit "struct TestVector {" >> newline
    indent 4
    forM_ l go
    unindent >> emit "}" >> newline
  where
    go (k, _, mdoc, rt) = do
        let str = keyToString k <> ": "
        forM_ mdoc $ \doc ->
            emit ("/// " <> doc) >> newline
        emit str >> indent (length str)
        case rt of
            CompatibleType t -> emitType t
            DefinedType (TypeName tn) -> emit tn
        emit "," >> unindent >> newline

newtype TypeName = TypeName String
  deriving (Show, Eq, Ord, Typeable)
instance IsString TypeName where
    fromString str = fromMaybe (error $ "Invalid TypeName" <> show str) (mkTypeName str)

mkTypeName :: LString -> Maybe TypeName
mkTypeName str = if isOk then Just (TypeName $ fromList str) else Nothing
  where
    !isOk = and $ check <$> str
    check c = elem c valids
    !valids = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> ['_']

data RustType
    = CompatibleType Type
    | DefinedType TypeName
  deriving (Show, Ord, Eq, Typeable)

newtype RustObject = RustObject [(Key, RustType)]
    deriving (Show, Eq, Ord, Typeable, Semigroup, Monoid, Collection, Sequential, IndexedCollection, Foldable)

type instance Element RustObject = (Key, RustType)

instance KeyedCollection RustObject where
    type Key RustObject   = Key
    type Value RustObject = RustType
    lookup k = F.lookup k . toList

instance IsList RustObject where
    type Item RustObject = (Key, RustType)
    toList (RustObject l) = l
    fromList = RustObject

newtype Convertor a = Convertor (StateT DefinedTypes Identity a)
  deriving (Functor, Applicative, Monad)
instance MonadState Convertor where
    type State Convertor = DefinedTypes
    withState = Convertor . withState

runConvertion :: Convertor a -> (a, DefinedTypes)
runConvertion (Convertor conv) = runIdentity $ runStateT conv mempty

isTypeDefined :: RustObject -> Convertor (Maybe TypeName)
isTypeDefined ro = withState $ \dts ->
    let r = F.find (\(_, t) -> t == ro) dts
     in (fst <$> r, dts)

mkDefinedType :: Key -> RustObject -> Convertor TypeName
mkDefinedType k ro = withState $ \dts ->
    let Just (x, xs) = F.uncons $ keyToString k
        tn = TypeName $ (upper $ singleton x) <> xs
     in (tn, F.cons (tn, ro) dts)

convertType :: Key -> Type -> Convertor RustType
convertType key ty = case ty of
    Type.Object obj -> do
        ro <- RustObject <$> forM (toList obj) traverseFields
        mdef <- isTypeDefined ro
        case mdef of
            Just def -> pure $ DefinedType def
            Nothing  -> DefinedType <$> mkDefinedType key ro
    _               -> pure $ CompatibleType ty
  where
    traverseFields (k, t) = do
        rt <- convertType k t
        pure (k, rt)

convertTestVector :: TestVector (Type, Value, Value) -> Convertor [(Key, Bool, Maybe String, RustType)]
convertTestVector tv = forM (snd <$> toList tv) $ \entry -> do
    let (t, _, _) = entryExtra entry
    rt <- convertType (entryKey entry) t
    pure (entryKey entry, fromMaybe undefined $ entryInput entry, entryDoc entry, rt)

newtype DefinedTypes = DefinedTypes [(TypeName, RustObject)]
  deriving (Show, Eq, Ord, Typeable, Semigroup, Monoid, Collection, Sequential, IndexedCollection, Foldable)
type instance Element DefinedTypes = (TypeName, RustObject)
instance KeyedCollection DefinedTypes where
    type Key DefinedTypes   = TypeName
    type Value DefinedTypes = RustObject
    lookup k = F.lookup k . toList
instance IsList DefinedTypes where
    type Item DefinedTypes = (TypeName, RustObject)
    toList (DefinedTypes l) = l
    fromList = DefinedTypes

emitType :: Type -> Builder ()
emitType t = case t of
    Type.Boolean    -> emit "bool"
    Type.Unsigned8  -> emit "u8"
    Type.Unsigned16 -> emit "u16"
    Type.Unsigned32 -> emit "u32"
    Type.Unsigned64 -> emit "u64"
    Type.Signed8    -> emit "i8"
    Type.Signed16   -> emit "i16"
    Type.Signed32   -> emit "i32"
    Type.Signed64   -> emit "i64"
    Type.Float32    -> emit "f32"
    Type.Float64    -> emit "f64"
    Type.String     -> emit "&'static str"
    Type.Array arr  -> emitArray arr
    Type.Object _   -> undefined
        -- TODO, this is a problem, we need to define objects
        -- ahead as they cannot be defined inline of the definition.

emitArray :: Type.Array -> Builder ()
emitArray (Type.SizedArray ty sz) =
    emit "[" >> emitType ty >> emit (";" <> show sz <> "]")
emitArray (Type.UnsizedArray ty) =
    emit "&'static [" >> emitType ty >> emit "]"

valueBuilder :: Key -> Value -> Builder ()
valueBuilder _ (Value.Boolean b)  = emit $ if b then "true" else "false"
valueBuilder _ (Value.Integer i)  = emit (show i)
valueBuilder _ (Value.Floating f) = emit (show f) -- TODO
valueBuilder _ (Value.String s)   = emit (show s)
valueBuilder k (Value.Array arr) = case toList arr of
        []     -> emit "&[]"
        [x]    -> emit "&[ " >> valueBuilder k x >> emit " ]"
        (x:xs) -> do
            emit "&[ " >> valueBuilder k x
            forM_ xs $ \v -> emit ", " >> valueBuilder k v
            emit "]"
valueBuilder k (Value.Object obj) = case toList obj of
    [] -> emit "{}"
    [(k1,v1)] -> emit tn >> emit " { " >> emit (keyToString k1) >> emit ": " >> valueBuilder k1 v1 >> emit " }"
    ((k1,v1):xs) -> do
        emit tn >> emit " {" >> emit (keyToString k1) >> emit ": " >> valueBuilder k1 v1
        forM_ xs $ \(k', v) -> emit ", " >> emit (keyToString k') >> emit ": " >> valueBuilder k' v
        emit "}"
  where
    Just (a, b) = F.uncons $ keyToString k
    tn = (upper $ singleton a) <> b
