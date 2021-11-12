{-|
Module      : Horrocubes.MintingScriptWithCounter.
Description : Mint policy for NFTs.
License     : Apache-2.0
Maintainer  : angel.castillob@protonmail.com
Stability   : experimental

This policy creates an NFT and uses an eUTXO with an internal counter to make the NFT truly unique.
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

module Horrocubes.MintingScriptWithCounter
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
import           Data.Aeson               (FromJSON, ToJSON)
import           GHC.Generics             (Generic)
import qualified Ledger.Contexts          as Validation
import           Text.Show

-- DATA TYPES -----------------------------------------------------------------

-- | The counter datum datatype.
data CounterDatum = CounterDatum {
        cdValue :: !Integer, -- ^ The current counter value.
        cdLimit :: !Integer  -- ^ The value limit, after this limit is reached, this eUTXO can not be spent again.
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''CounterDatum

-- DEFINITIONS ----------------------------------------------------------------

-- | Zero pads a given hex value to 8 cahracters.
{-# INLINABLE padLeft #-}
padLeft :: BuiltinByteString -> BuiltinByteString  -> BuiltinByteString
padLeft charset bs = if lengthOfByteString bs < 8
  then  padLeft charset (consByteString (indexByteString charset 0) bs)
  else bs

-- | Gets the Hash of the given UTXO.
{-# INLINABLE utxoHash #-}
utxoHash:: TxOutRef -> BuiltinByteString
utxoHash utxo = getTxId $ txOutRefId utxo

-- | Encodes an Integer into a diffent base (ie base 16).
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
mkNFTPolicy :: BuiltinByteString -> PubKeyHash -> AssetClass -> BuiltinData -> ScriptContext -> Bool
mkNFTPolicy charset pkh identityNft _ ctx  = 
        traceIfFalse "Identity NFT not found"          isIdentityNftSpent &&
        traceIfFalse "Invalid Postfix or wrong amount" checkMintedAmount &&
        traceIfFalse "Missing signature"               isTransactionSignedByOwner
    where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      tokenNameToByteString :: TokenName -> BuiltinByteString
      tokenNameToByteString tn = unTokenName tn

      actuallPosfix :: BuiltinByteString -> BuiltinByteString
      actuallPosfix tn = sliceByteString ((lengthOfByteString tn) - 8) 8 $ tn

      expectedPosfix :: BuiltinByteString
      expectedPosfix = padLeft charset $ encodeBase charset $ datumIntegerValue

      isIdentityNftSpent :: Bool
      isIdentityNftSpent = assetClassValueOf valueSpentByScript identityNft == 1

      checkMintedAmount :: Bool
      checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> (equalsByteString (actuallPosfix $ tokenNameToByteString tn') expectedPosfix) && amt == 1
        _               -> False

      valueSpentByScript :: Value
      valueSpentByScript = Validation.valueSpent info

      isTransactionSignedByOwner :: Bool
      isTransactionSignedByOwner = txSignedBy info pkh

      findUtxoWithIdentityNft :: TxOut
      findUtxoWithIdentityNft = case filter (\(TxOut{txOutValue}) -> assetClassValueOf txOutValue identityNft == 1) (txInfoOutputs info) of
        [o] -> o
        _   -> traceError "Expected exactly one output"

      stateDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe CounterDatum
      stateDatum o f = do
        dh      <- txOutDatum o
        Datum d <- f dh
        PlutusTx.fromBuiltinData d

      datumIntegerValue :: Integer
      datumIntegerValue = case stateDatum findUtxoWithIdentityNft (`findDatum` info) of
        Nothing -> traceError "Counter output datum not found"
        Just datum -> cdValue datum

 -- | Compiles the policy.
nftPolicy :: BuiltinByteString -> PubKeyHash -> AssetClass -> Scripts.MintingPolicy
nftPolicy charset pkh ac = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \charset' pkh' ac' -> Scripts.wrapMintingPolicy $ mkNFTPolicy charset' pkh' ac'||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode charset
    `PlutusTx.applyCode`
     PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
     PlutusTx.liftCode ac

-- | Generates the plutus script.
nftPlutusScript :: BuiltinByteString -> PubKeyHash -> AssetClass -> Script
nftPlutusScript charset pkh ac = unMintingPolicyScript $ nftPolicy charset pkh ac

-- | Generates the NFT validator.
nftValidator :: BuiltinByteString -> PubKeyHash  -> AssetClass -> Validator
nftValidator charset pkh ac = Validator $  nftPlutusScript charset pkh ac

-- | Serializes the contract in CBOR format.
nftScriptAsCbor :: BuiltinByteString -> PubKeyHash -> AssetClass -> LB.ByteString
nftScriptAsCbor charset pkh ac = serialise $ nftValidator charset pkh ac

-- | Serializes the contract in CBOR format.
nftScriptShortBs :: BuiltinByteString -> PubKeyHash -> AssetClass -> SBS.ShortByteString
nftScriptShortBs charset pkh ac = SBS.toShort . LB.toStrict $ nftScriptAsCbor charset pkh ac

-- | Gets a serizlize plutus script from the given UTXO and token name.
mintScript :: BuiltinByteString -> PubKeyHash -> AssetClass -> PlutusScript PlutusScriptV1
mintScript charset pkh ac = PlutusScriptSerialised . SBS.toShort . LB.toStrict $ nftScriptAsCbor charset pkh ac