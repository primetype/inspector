{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Inspector.Export.Markdown
    ( run
    ) where

import Foundation
import Foundation.Collection (nonEmpty_)
import Foundation.Monad
import Foundation.IO (withFile, IOMode(..), hPut)
import Foundation.VFS.FilePath
import Foundation.String (toBytes, Encoding(UTF8))

import Control.Monad (forM_, mapM_)

import Inspector.Monad (GoldenT, Config(..), ask, mkPath)
import Inspector.Builder
import Inspector.TestVector.TestVector (TestVector, Entry(..), inputs, outputs)
import Inspector.TestVector.Value (Value, valueBuilder)
import Inspector.TestVector.Types (Type)
import Inspector.TestVector.Key (keyToString)

run :: FilePath -> [(Word, TestVector (Type, Value, Value))] -> GoldenT ()
run path tvs = do
    stdout <- getStdout <$> ask
    fp <- mkfp

    let out = runBuilder $ buildMarkdown path tvs

    liftIO $ if stdout
        then putStr out
        else withFile fp  WriteMode $ flip hPut (toBytes UTF8 out)
  where
    mkfp = do
        fp <- mkPath path
        pure $ fromString $ toList $ (filePathToString fp) <> ".md"

buildMarkdown :: FilePath -> [(Word, TestVector (Type, Value, Value))] -> Builder ()
buildMarkdown path tvs = do
    emit ("# " <> (filePathToString path)) >> newline
    newline
    defineTestVector $ snd $ head $ nonEmpty_ tvs
    newline
    emit "## Test Vectors" >> newline
    newline
    forM_ tvs $ \(testNumber, tv) -> declareTestVector testNumber tv

declareTestVector :: Word -> TestVector (Type, Value, Value) -> Builder ()
declareTestVector n tv = do
    emit ("### Test vector n°" <> show n) >> newline
    newline
    emit "```" >> newline
    mapM_ go (inputs tv)
    newline
    mapM_ go (outputs tv)
    emit "```" >> newline
    newline
  where
    go e = do
        let (t, v, _) = entryExtra e
        let str = keyToString (entryKey e) <> " = "
        emit str
        indent (length str)
        valueBuilder v t
        unindent
        newline

defineTestVector :: TestVector (Type, Value, Value) -> Builder ()
defineTestVector tv = do
    go "Inputs" (inputs tv)
    newline
    go "Outputs" (outputs tv)
  where
    go ty l = do
        emit ("## " <> ty) >> newline
        newline
        emit "```" >> newline
        forM_ l $ \e -> do
            emit $ (keyToString $ entryKey e) <> " = " <> fromMaybe "No documentation" (entryDoc e)
            newline
        emit "```" >> newline