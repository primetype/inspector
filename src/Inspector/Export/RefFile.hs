
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inspector.Export.RefFile
    ( testVectorSourceC
    , testVectorSinkC
    , parseTestVectorFile
    , writeTestVectorFile
    , run
    ) where

import Foundation
import Foundation.IO
import Foundation.Monad
import Foundation.Conduit
import Foundation.Conduit.Textual
import Foundation.String (Encoding(UTF8))

import Foundation.VFS.FilePath
import qualified Foundation.Parser as Parser

import           Inspector.TestVector.Types (Type)
import           Inspector.TestVector.Value (Value)
import           Inspector.TestVector.TestVector (TestVector, Entry(..), testVectorBuilder, testVectorParser)
import           Inspector.Builder
import           Inspector.Monad
import           Inspector.Parser

import Data.List (zip)

run :: FilePath -> [(Word, TestVector (Type, Value, Value))] -> GoldenT ()
run path tvs = do
    needStdout <- getStdout <$> ask
    fp <- mkPath path

    let tvs' = flip fmap tvs $ \(w, tv) -> (w, fromList $ filterEntry <$> toList tv)

    liftIO $ if needStdout
        then runConduit $ yields tvs' .| testVectorSinkC .| toBytes UTF8 .| sinkHandle stdout
        else withFile fp WriteMode $ \h -> runConduit $
                yields tvs' .| testVectorSinkC .| toBytes UTF8 .| sinkHandle h
  where
    filterEntry (k, e) = (k,e')
      where
        e' = Entry
            { entryKey = entryKey e
            , entryType = entryType e
            , entryInput = entryInput e
            , entryValue = entryValue e
            , entryDoc = entryDoc e
            , entryExtra = let (a,_,c) = entryExtra e in (a,c)
            }

writeTestVectorFile :: FilePath -> [TestVector (Type, Value)] -> IO ()
writeTestVectorFile fp tvs = withFile fp WriteMode $ \h -> runConduit $
    yields (zip [1..] tvs) .| testVectorSinkC .| toBytes UTF8 .| sinkHandle h

testVectorSinkC :: Monad m => Conduit (Word, TestVector (Type, Value)) String m ()
testVectorSinkC =  awaitForever $ \(w, tv) -> do
    yield $ "# Test Vector " <> show w <> "\n"
    yield $ runBuilder $ testVectorBuilder tv

parseTestVectorFile :: FilePath -> IO [TestVector ()]
parseTestVectorFile fp = withFile fp ReadMode $ \h -> runConduit $
    sourceHandle h .| fromBytes UTF8 .| testVectorSourceC .| sinkList

testVectorSourceC :: Monad m => Conduit String (TestVector ()) m ()
testVectorSourceC = go defaultS
  where
    go st = do
        mstr <- await
        case mstr of
            Nothing -> pure ()
            Just str ->
                case parse st (Parser.optional testVectorParser) str of
                    ParseOk str' (Just r, st') -> leftover str' >> yield r >> go st'
                    ParseOk str' (Nothing, st') -> error $ show ("not enought (parseDict)" :: String, str', st')
                    ParseFailed err         -> error $ show err
                    ParseMore   more        -> go' more
    go' more = do
        mstr <- await
        case mstr of
            Nothing -> case more mempty of
                ParseOk str' (Just r, st) -> leftover str' >> yield r >> go st
                ParseOk _ (Nothing, _) -> pure () -- error $ show ("not enought (more empty)", str', st)
                ParseFailed err  -> error $ show err
                ParseMore   _ -> error "ParserMore (more mempty)"
            Just str -> case more str of
                ParseOk str' (Just r, st) -> leftover str' >> yield r >> go st
                ParseOk str' (Nothing, st) -> error $ show ("not enought (more str)" :: String, str, str', st)
                ParseFailed err  -> error $ show err
                ParseMore   more' -> go' more'
