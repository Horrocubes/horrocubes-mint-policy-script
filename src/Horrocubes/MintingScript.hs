{-|
Module      : Horrocubes.MintingScript.
Description : Mint policy for NFTs.
License     : Apache-2.0
Maintainer  : angel.castillob@protonmail.com
Stability   : experimental

This policy creates an NFT and uses an UTXO to make the NFT truly unique.
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
import qualified Data.ByteString.Char8    as C
import           PlutusTx.Builtins 
import Horrocubes.Deserialisation

-- DEFINITIONS ----------------------------------------------------------------

{- HLINT ignore "Avoid lambda" -}

-- | Gets the Hash of the given UTXO.
{-# INLINABLE utxoHash #-}
utxoHash:: TxOutRef -> BuiltinByteString
utxoHash utxo = getTxId $ txOutRefId utxo

-- | Encodes an Integer into a diffent base (ie base 64).
{-# INLINABLE encodeBase #-}
encodeBase :: BuiltinByteString -> Integer -> BuiltinByteString
encodeBase charset value = encoded where
  base     = lengthOfByteString charset
  encoded  = expand (value `divMod` base) emptyByteString
  lookup n = indexByteString charset n
  expand (dividend, rem) xs
    | (dividend >  0) = expand (dividend `divMod` base) result
    | (dividend == 0 && rem >  0) = result
    | (dividend == 0 && rem == 0) = xs
    where result = consByteString (lookup rem) xs

-- | Creates the minting script for the NFT.
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: BuiltinByteString -> PubKeyHash -> BuiltinData -> ScriptContext -> Bool
mkNFTPolicy charset _ _ ctx = traceIfFalse "Invalid Postfix" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    expectedPosfix :: BuiltinByteString
    expectedPosfix = encodeBase charset $ builtinByteStringToInt 0 0 $ sliceByteString 16 16 $ utxoHash getUTxO

    actuallPosfix :: TokenName -> BuiltinByteString
    actuallPosfix tn = sliceByteString 10 (lengthOfByteString expectedPosfix) $ unTokenName tn

    getUTxO :: TxOutRef
    getUTxO = txInInfoOutRef $ ((txInfoInputs info) !! 0)

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> (equalsByteString (actuallPosfix tn') expectedPosfix) && amt == 1
        _               -> False

-- | Compiles the policy.
nftPolicy :: BuiltinByteString -> PubKeyHash -> Scripts.MintingPolicy
nftPolicy charset pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \charset' pkh' -> Scripts.wrapMintingPolicy $ mkNFTPolicy charset' pkh' ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode charset
    `PlutusTx.applyCode`
     PlutusTx.liftCode pkh

-- | Generates the plutus script.
nftPlutusScript :: BuiltinByteString -> PubKeyHash -> Script
nftPlutusScript charset pkh = unMintingPolicyScript $ nftPolicy charset pkh

-- | Generates the NFT validator.
nftValidator :: BuiltinByteString -> PubKeyHash  -> Validator
nftValidator charset pkh = Validator $  nftPlutusScript charset pkh

-- | Serializes the contract in CBOR format.
nftScriptAsCbor :: BuiltinByteString -> PubKeyHash -> LB.ByteString
nftScriptAsCbor charset pkh = serialise $ nftValidator charset pkh

-- | Serializes the contract in CBOR format.
nftScriptShortBs :: BuiltinByteString -> PubKeyHash -> SBS.ShortByteString
nftScriptShortBs charset pkh = SBS.toShort . LB.toStrict $ nftScriptAsCbor charset pkh

-- | Gets a serizlize plutus script from the given UTXO and token name.
mintScript :: BuiltinByteString -> PubKeyHash -> PlutusScript PlutusScriptV1
mintScript charset pkh = PlutusScriptSerialised . SBS.toShort . LB.toStrict $ nftScriptAsCbor charset pkh