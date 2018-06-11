{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Inspector
import qualified Inspector.TestVector.Types as Type
import qualified Inspector.TestVector.Value as Value

import Foundation
import Foundation.Check (Arbitrary(..))
import Data.ByteArray (Bytes)

import Crypto.Hash
import Crypto.KDF.PBKDF2 (fastPBKDF2_SHA1, Parameters (..))

type GoldenSHA1   = "hash" :> "SHA1"   :> Payload "payload" String :> Payload "hash" (Digest SHA1)
type GoldenSHA256 = "hash" :> "SHA256" :> Payload "payload" String :> Payload "hash" (Digest SHA256)

type GoldenPBKDF2 = "kdf" :> "PBKDF2" :> "SHA1"
                 :> Payload "parameters" Parameters :> Payload "password" String :> Payload "salt" String :> Payload "hash" Bytes

main :: IO ()
main = defaultTest $ do
    group $ do
        summary "Secure Hash Algorithm"
        golden (Proxy @GoldenSHA1) hash
        golden (Proxy @GoldenSHA256) hash
    group $ do
        summary "Password-Based Key Derivation"
        golden (Proxy @GoldenPBKDF2) $ \params pwd salt ->
            fastPBKDF2_SHA1 params pwd salt

instance Arbitrary Parameters where
    arbitrary = undefined

instance Inspectable Parameters where
    documentation _ = "PBKDF2 Parameters."
    exportType _ = Type.Object $ Type.ObjectDef
        [ ("iter", Type.Signed64)
        , ("len", Type.Signed64)
        ]
    builder (Parameters iter len) = Value.Object $ Value.ObjectDef
        [ ("iter", builder iter)
        , ("len", builder len)
        ]
    parser = withStructure "Parameters" $ \obj -> do
        iter <- parser =<< field obj "iter"
        len <- parser =<< field obj "len"
        pure $ Parameters iter len
