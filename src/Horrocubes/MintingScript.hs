{-|
Module      : Horrocubes.MintingScript.
Description : Mint policy for NFTs.
License     : Apache-2.0
Maintainer  : angel.castillob@protonmail.com
Stability   : experimental

This policy creates an NFT and uses an UTXO to make the NFT truly unique.

This minting policy was taken from https://github.com/input-output-hk/lobster-challenge
and sightly modified to the the token name as an outside parameter.

-}

-- LANGUAGE EXTENSIONS --------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
 {-# LANGUAGE OverloadedStrings #-}

-- MODULE DEFINITION ----------------------------------------------------------

module Horrocubes.MintingScript
(
  mintScript,
  nftScriptShortBs
) where

-- IMPORTS --------------------------------------------------------------------

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString
import           Data.Char (ord)
import           Data.List (elemIndex)
import Data.Char
import Data.Word
import Data.Bits (shift, (.|.))
import qualified Prelude
import GHC.Generics

-- DEFINITIONS ----------------------------------------------------------------

{-# INLINABLE alphabetBase89 #-}
alphabetBase89:: [Char]
alphabetBase89 = "!~}|{zyxwvutsrqponmlkjihgfedcba_^]#[ZYXWVUTSRQPONMLKJIHGFEDCBA@?>=<;:9876543210/.-+*)($&%"

{-# INLINABLE encodeBase #-}
encodeBase :: Integer -> C.ByteString
encodeBase value = C.pack encoded where
  base     = 89
  encoded  = expand (value `divMod` base) []
  lookup n = alphabetBase89 !! (Prelude.fromIntegral n)
  expand (dividend, remainder) xs
    | (dividend >  0) = expand (dividend `divMod` base) result
    | (dividend == 0 && remainder >  0) = result
    | (dividend == 0 && remainder == 0) = xs
    where result = [lookup remainder] ++ xs

{-# INLINABLE slice #-}
slice :: Prelude.Int -> Prelude.Int -> C.ByteString -> C.ByteString
slice start len = C.take len . C.drop start

{-# INLINABLE readInt #-}
readInt :: C.ByteString -> Integer
readInt bs =     (byte 0 `shift` 120)
             .|. (byte 1  `shift` 112)
             .|. (byte 2  `shift` 104)
             .|. (byte 3  `shift` 96)
             .|. (byte 4  `shift` 88)
             .|. (byte 5  `shift` 80)
             .|. (byte 6  `shift` 72)
             .|. (byte 7  `shift` 64)
             .|. (byte 8  `shift` 56)
             .|. (byte 9  `shift` 48)
             .|. (byte 10 `shift` 40)
             .|. (byte 11 `shift` 32)
             .|. (byte 12 `shift` 24)
             .|. (byte 13 `shift` 16)
             .|. (byte 14 `shift` 8)
             .|.  byte 15
        where byte n = Prelude.fromIntegral $ ord (bs `C.index` n)

{- HLINT ignore "Avoid lambda" -}

-- | Creates the minting script for the NFT.
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: TokenName -> TxOutRef -> BuiltinData -> ScriptContext -> Bool
mkNFTPolicy tn utxo _ ctx = traceIfFalse "Token posfix invalid" ((length (filter (\x -> Prelude.notElem x tokenEnding) tokenPosifx)) == 0) &&
                            traceIfFalse "Wrong amount minted" checkMintedAmount
  where
    tokenName:: C.ByteString
    tokenName = Plutus.fromBuiltin $ Plutus.unTokenName tn

    tokenEnding:: [Char]
    tokenEnding = C.unpack $ slice 10 20 tokenName

    tokenPosifx:: [Char]
    tokenPosifx = C.unpack $ encodeBase $ readInt $ Plutus.fromBuiltin $ getTxId $ txOutRefId utxo

    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

-- | Compiles the policy.
nftPolicy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
nftPolicy utxo nftTokenName = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \tn utxo' ->   Scripts.wrapMintingPolicy $ mkNFTPolicy tn utxo' ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode nftTokenName
    `PlutusTx.applyCode`
     PlutusTx.liftCode utxo


-- | Generates the plutus script.
nftPlutusScript :: TxOutRef -> TokenName -> Script
nftPlutusScript utxo tn = unMintingPolicyScript $ nftPolicy utxo tn

-- | Generates the NFT validator.
nftValidator :: TxOutRef -> TokenName -> Validator
nftValidator utxo tn = Validator $  nftPlutusScript utxo tn

-- | Serializes the contract in CBOR format.
nftScriptAsCbor :: TxOutRef -> TokenName -> LB.ByteString
nftScriptAsCbor utxo tn = serialise $ nftValidator utxo tn

-- | Serializes the contract in CBOR format.
nftScriptShortBs :: TxOutRef -> BuiltinByteString -> SBS.ShortByteString
nftScriptShortBs utxo tn = SBS.toShort . LB.toStrict $ nftScriptAsCbor utxo $ toTokenName tn

-- | Gets a serizlize plutus script from the given UTXO and token name.
mintScript :: TxOutRef -> BuiltinByteString -> PlutusScript PlutusScriptV1
mintScript utxo tn = PlutusScriptSerialised . SBS.toShort . LB.toStrict $ nftScriptAsCbor utxo $ toTokenName tn

-- | Creates a token name from a byte array.
toTokenName :: BuiltinByteString -> TokenName
toTokenName tn = TokenName { unTokenName = tn }