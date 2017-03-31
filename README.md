# inspector

Futuristic Golden Test Managements.

Define the Inspector script, such as the following:

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

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
```

And now write the appropriate test vectors:

for example, for the SHA1's Test Vectors:

* First you create the initial test vectors with the input only:

```shell
TestVector
payload = ""

TestVector
payload = "The quick brown fox jumps over the lazy dog"

TestVector
payload = "The quick brown fox jumps over the lazy cog"
```

* then you execute the Inspector script with the `generate` option.
  It will provide the following output:

```shell
# Test Vector 1
TestVector
hash = "da39a3ee5e6b4b0d3255bfef95601890afd80709"
payload = ""

# Test Vector 2
TestVector
hash = "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12"
payload = "The quick brown fox jumps over the lazy dog"

# Test Vector 3
TestVector
hash = "de9f2c7fd25e1b3afad3e85a0bd17d9b100db4b3"
payload = "The quick brown fox jumps over the lazy cog"

```

# Future enhancements:

- [ ] generate pretty Markdown output
- [ ] generate ready to use C test vectors
- [ ] generate ready to use Rust test vectors
- [ ] generate ready to use JS test vectors
