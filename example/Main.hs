{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Inspector

import Foundation
import Data.ByteArray (Bytes)

import Crypto.Hash
import Crypto.KDF.PBKDF2 (fastPBKDF2_SHA1, Parameters (..))

type GoldenSHA1   = "hash" :> "SHA1"   :> Payload "payload" String :> Payload "hash" (Digest SHA1)
type GoldenSHA256 = "hash" :> "SHA256" :> Payload "payload" String :> Payload "hash" (Digest SHA256)

type GoldenPBKDF2 = "kdf" :> "PBKDF2" :> "SHA1"
                 :> Payload "iter" Int :> Payload "size" Int :> Payload "password" String :> Payload "salt" String :> Payload "hash" Bytes

main :: IO ()
main = defaultMain $ do
    golden (Proxy @GoldenSHA1) hash
    golden (Proxy @GoldenSHA256) hash
    golden (Proxy @GoldenPBKDF2) $ \iter len pwd salt ->
        fastPBKDF2_SHA1 (Parameters iter len) pwd salt
