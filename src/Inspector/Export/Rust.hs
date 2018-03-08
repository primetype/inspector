{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Inspector.Export.Rust
    ( pop
    , summary
    ) where

import Foundation
import Foundation.Monad
import Foundation.VFS
import Foundation.VFS.FilePath
import Foundation.IO
import Foundation.Conduit
import Foundation.Conduit.Textual
import Foundation.String (Encoding(UTF8))

import Inspector.Dict
import Inspector.Monad hiding (summary)
import Inspector.Report
import Inspector.Method

import Inspector.Export.Types

import Control.Monad (forM_)
import Data.List (zip)
import GHC.TypeLits

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
         "/// " <> key <> " (" <> ty <> ") = " <> show enc <> "\n"
    yield "/// ```\n"
    yield "///\n"
    yield "/// ## Output(s)\n///\n"
    yield "/// ```\n"
    yields $ for outputs $ \(Description key enc _ ty _) ->
         "/// " <> key <> " (" <> ty <> ") = " <> show enc <> "\n"
    yield "/// ```\n"
    yield "struct TestVector {\n  "
    yields $ intersperse ",\n  " $ for inputs $ \(Description key _ _ ty _) ->
         key <> " : " <> ty
    yield ",\n  "
    yields $ intersperse ",\n  " $ for outputs $ \(Description key enc _ ty _) ->
         key <> " : " <> ty
    yield "\n}\n\n"
  where
    path = filePathToString $ unsafeFilePath Relative (getPath p)
    Export inputs outputs = describe p Rust

pop :: (Monad m, Golden method) => Proxy method -> Conduit (Word, Dict) String m ()
pop p = awaitForever $ \(idx, dic) -> do
    let is = findKeyVal dic inputs
    let os = findKeyVal dic outputs
    yield $ if idx == 1 then "  [ TestVector" else "  , TestVector"
    yield "\n    { "
    yields $ intersperse "\n    , " $ for is $ \(k,v) -> k <> " : " <> v
    yield "\n    , "
    yields $ intersperse "\n    , " $ for os $ \(k,v) -> k <> " : " <> v
    yield "\n    }\n"
  where
    Export inputs outputs = describe p Rust

for :: [a] -> (a -> b) -> [b]
for = flip fmap

findKeyVal :: Dict -> [Description] -> [(String, String)]
findKeyVal _ [] = []
findKeyVal d (Description k _ _ _ _:xs) = case lookup k d of
    Nothing -> error $ "missing input: " <> k
    Just v  -> (k, v) : findKeyVal d xs
