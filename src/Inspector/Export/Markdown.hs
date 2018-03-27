{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Inspector.Export.Markdown
    ( pop
    , summary
    ) where

import Foundation
import Foundation.Monad
import Foundation.VFS
import Foundation.VFS.FilePath

import Foundation.Conduit


import Foundation.Collection (Element)

import Inspector.Dict
import Inspector.Monad hiding (summary)

import Inspector.Method

import Inspector.Export.Types





summary :: Golden method
        => Proxy method
        -> Conduit a String GoldenT ()
summary p = do
    meta <- lift getMetadata
    yield $ "# " <> path <> "\n\n"
    yield (metaDescription meta <> "\n")
    yield "\n"
    yield "## Input(s)\n\n"
    yield "```\n"
    yields $ for inputs $ \(Description key enc ty _ mcomm) ->
        let comm = maybe "" (" # " <>) mcomm
         in key <> " (" <> show ty <> ") = " <> show enc <> comm <> "\n"
    yield "```\n"
    yield "\n"
    yield "## Output(s)\n\n"
    yield "```\n"
    yields $ for outputs $ \(Description key enc ty _ mcomm) ->
        let comm = maybe "" (" # " <>) mcomm
         in key <> " (" <> show ty <> ") = " <> show enc <> comm <> "\n"
    yield "```\n"
    yield "\n"
    yield "# Test vectors\n\n"
  where
    path = filePathToString $ unsafeFilePath Relative (getPath p)
    Export inputs outputs = describe p

pop :: (Monad m, Golden method) => Proxy method -> Conduit (Word, Dict) String m ()
pop p = awaitForever $ \(idx, dic) -> do
    let is = findKeyVal dic inputs
    let os = findKeyVal dic outputs
    yield $ "## Test vector " <> show idx <> "\n\n"
    yield "```\n"
    yields $ for is $ \(k,v) -> k <> " = " <> buildIntermediarType "" v <> "\n"
    yield "\n"
    yields $ for os $ \(k,v) -> k <> " = " <> buildIntermediarType "" v <> "\n"
    yield "```\n"
    yield "\n"
  where
    Export inputs outputs = describe p

for :: [a] -> (a -> b) -> [b]
for = flip fmap

findKeyVal :: Dict -> [Description] -> [Element Dict]
findKeyVal _ [] = []
findKeyVal d (Description k _ _ _ _:xs) = case lookup k d of
    Nothing -> error $ "missing input: " <> k
    Just v  -> (k, v) : findKeyVal d xs

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
