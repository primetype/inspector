{-# LANGUAGE OverloadedStrings #-}

module Inspector.Export.Markdown
    ( exportMarkdown
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
import Inspector.Display
import Inspector.Monad
import Inspector.Report
import Inspector.Method

import Inspector.Export.Types

import Control.Monad (forM_)
import Data.List (zip)

exportMarkdown :: (MonadIO io, Golden method)
               => Proxy method
               -> FilePath
               -> Metadata
               -> [Dict]
               -> io ()
exportMarkdown p file meta dics = liftIO $ withFile mdfile WriteMode $ \h ->
    runConduit $ runExport .| toBytes UTF8 .| sinkHandle h
  where
    mdfile = fromString (filePathToLString file <> ".md")
    Export inputs outputs = describe p

    runExport :: Conduit () String IO ()
    runExport = do
        exportDescription
        exportDics dics

    for :: [a] -> (a -> b) -> [b]
    for = flip fmap

    exportDescription :: Conduit () String IO ()
    exportDescription = do
        yield "# Summary\n\n"
        yield (metaDescription meta <> "\n")
        yield "\n"
        yield "## Input(s)\n\n"
        yield "```\n"
        yields $ for inputs $ \(Description key enc ty mcomm) ->
            let comm = maybe "" (" # " <>) mcomm
             in key <> " (" <> show ty <> ") = " <> enc <> comm <> "\n"
        yield "```\n"
        yield "\n"
        yield "## Output(s)\n\n"
        yield "```\n"
        yields $ for outputs $ \(Description key enc ty mcomm) ->
            let comm = maybe "" (" # " <>) mcomm
             in key <> " (" <> show ty <> ") = " <> enc <> comm <> "\n"
        yield "```\n"
        yield "\n"
    exportDics dics = do
        yield "# Test vectors\n\n"
        forM_ (zip [1..] dics) $ \(idx, dic) -> do
            let is = findKeyVal dic inputs
            let os = findKeyVal dic outputs

            yield $ "# Test vector " <> show idx <> "\n\n"
            yield "```\n"
            yields $ for is $ \(k,v) -> k <> " = " <> v <> "\n"
            yield "\n"
            yields $ for os $ \(k,v) -> k <> " = " <> v <> "\n"
            yield "```\n"
            yield "\n"

findKeyVal :: Dict -> [Description] -> [(String, String)]
findKeyVal _ [] = []
findKeyVal d (Description k _ _ _:xs) = case lookup k d of
    Nothing -> error $ "missing input: " <> k
    Just v  -> (k, v) : findKeyVal d xs
