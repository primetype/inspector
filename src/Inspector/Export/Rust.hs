{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Inspector.Export.Rust
    ( pop
    , summary
    ) where

import Foundation
import Foundation.Collection (Element)
import Foundation.Monad
import Foundation.VFS
import Foundation.VFS.FilePath

import Foundation.Conduit



import Inspector.Dict
import Inspector.Monad hiding (summary)

import Inspector.Method

import Inspector.Export.Types





exportTypeToRust :: ExportType -> String
exportTypeToRust t = case t of
    TypeBoolean -> "bool"
    TypeSignedInteger ts -> "i" <> sizeToRust ts
    TypeUnsignedInteger ts -> "u" <> sizeToRust ts
    TypeDouble -> "f64"
    TypeString -> "&'static str"
    TypeArray et Nothing -> "&' static ["<>exportTypeToRust et<>"]"
    TypeArray et (Just n) -> "["<>exportTypeToRust et<>";" <> show (fromCount n) <>"]"
    TypeStruct [] -> ""
    TypeStruct _xs -> undefined -- TODO !
  where
    sizeToRust Size8   =   "8"
    sizeToRust Size16  =  "16"
    sizeToRust Size32  =  "32"
    sizeToRust Size64  =  "64"
    sizeToRust Size128 = "128"
    sizeToRust Size256 = "256"

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
    ITStructure  [(n,v)] -> "{ " <> n <> " : " <> buildIntermediarType (alignment <> replicate (length n) ' ' <> "     ") v <> " }"
    ITStructure  str -> "{ "
                     <> intercalate ("\n"<> alignment <> ", ")
                                    ((\(n, v) -> n <> " : " <> buildIntermediarType (alignment <> replicate (length n) ' ' <> "     ") v) <$> str)
                     <> "\n" <> alignment <> "}"

summary :: Golden method
        => Proxy method
        -> Conduit a String GoldenT ()
summary p = do
    meta <- lift getMetadata
    yield $ "/// # GoldenTests: " <> path <> "\n///\n"
    yield $ "/// " <> (metaDescription meta <> "\n")
    yield "///\n"
    yield "/// ## Input(s)\n///\n"
    yield "/// ```\n"
    yields $ for inputs $ \(Description key enc _ ty _) ->
         "/// " <> key <> " (" <> exportTypeToRust ty <> ") = " <> show enc <> "\n"
    yield "/// ```\n"
    yield "///\n"
    yield "/// ## Output(s)\n///\n"
    yield "/// ```\n"
    yields $ for outputs $ \(Description key enc _ ty _) ->
         "/// " <> key <> " (" <> exportTypeToRust ty <> ") = " <> show enc <> "\n"
    yield "/// ```\n"
    yield "struct TestVector {\n  "
    yields $ intersperse ",\n  " $ for inputs $ \(Description key _ _ ty _) ->
         key <> " : " <> exportTypeToRust ty
    yield ",\n  "
    yields $ intersperse ",\n  " $ for outputs $ \(Description key _ _ ty _) ->
         key <> " : " <> exportTypeToRust ty
    yield "\n}\n\n"
  where
    path = filePathToString $ unsafeFilePath Relative (getPath p)
    Export inputs outputs = describe p

pop :: (Monad m, Golden method) => Proxy method -> Conduit (Word, Dict) String m ()
pop p = awaitForever $ \(idx, dic) -> do
    let is = findKeyVal dic inputs
    let os = findKeyVal dic outputs
    yield $ if idx == 1 then "  [ TestVector" else "  , TestVector"
    yield "\n    { "
    yields $ intersperse "\n    , " $ for is $ \(k,v) -> k <> " : " <> buildIntermediarType "" v
    yield "\n    , "
    yields $ intersperse "\n    , " $ for os $ \(k,v) -> k <> " : " <> buildIntermediarType "" v
    yield "\n    }\n"
  where
    Export inputs outputs = describe p

for :: [a] -> (a -> b) -> [b]
for = flip fmap

findKeyVal :: Dict -> [Description] -> [Element Dict]
findKeyVal _ [] = []
findKeyVal d (Description k _ _ _ _:xs) = case lookup k d of
    Nothing -> error $ "missing input: " <> k
    Just v  -> (k, v) : findKeyVal d xs
