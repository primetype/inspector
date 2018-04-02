{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Inspector.Export.Rust
    ( run
    ) where

import Foundation
import Foundation.VFS.FilePath (FilePath)
import Foundation.Collection (nonEmpty_)
import Foundation.Monad

import Control.Monad (forM_, when)

import Inspector.Monad (GoldenT)
import Inspector.Builder
import Inspector.Export.Types (liftValue)
import Inspector.TestVector.TestVector (TestVector, Entry(..))
import Inspector.TestVector.Value (Value)
import qualified Inspector.TestVector.Value as Value
import Inspector.TestVector.Types (Type)
import qualified Inspector.TestVector.Types as Type
import Inspector.TestVector.Key (keyToString)

run :: FilePath -> [(Word, TestVector (Type, Value, Value))] -> GoldenT ()
run _ tvs = liftIO $ putStrLn $ runBuilder $ buildRust tvs

buildRust :: [(Word, TestVector (Type, Value, Value))] -> Builder ()
buildRust tvs = do
    defineType $ snd $ head $ nonEmpty_ tvs
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
        forM_ (snd <$> toList tv) $ \ent -> do
            let str = keyToString (entryKey ent)
            let (t, v, _) = entryExtra ent
            emit str >> emit ": " >> valueBuilder (liftValue t v) >> emit "," >> newline
        unindent >> emit "  }" >> newline
        go True xs

defineType :: TestVector (Type, Value, Value) -> Builder ()
defineType tv = do
    emit "#[derive(Debug)]" >> newline
    emit "struct TestVector {" >> newline
    indent 4
    forM_ (snd <$> toList tv) $ \ent -> do
        let (t, _, _) = entryExtra ent
        let str = keyToString (entryKey ent) <> ": "
        emit str >> indent (length str) >> emitType t >> emit "," >> unindent >> newline
    unindent >> emit "}" >> newline

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

valueBuilder :: Value -> Builder ()
valueBuilder (Value.Boolean b)  = emit $ if b then "true" else "false"
valueBuilder (Value.Integer i)  = emit (show i)
valueBuilder (Value.Floating f) = emit (show f) -- TODO
valueBuilder (Value.String s)   = emit (show s)
valueBuilder (Value.Array arr) = case toList arr of
        []     -> emit "[]"
        [x]    -> emit "[ " >> valueBuilder x >> emit " ]"
        (x:xs) -> do
            emit "[ " >> valueBuilder x
            forM_ xs $ \v -> emit ", " >> valueBuilder v
            emit "]"
valueBuilder (Value.Object _) = undefined