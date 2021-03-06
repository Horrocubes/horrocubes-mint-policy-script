{-|
Module      : Horrocubes.Counter.
Description : Plutus script that keeps track of an internal counter.
License     : Apache-2.0
Maintainer  : angel.castillo@horrocubes.io
Stability   : experimental

This script keeps a counter and increases it everytime the eUTXO is spent.
-}

-- LANGUAGE EXTENSIONS --------------------------------------------------------

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ViewPatterns               #-}

-- MODULE DEFINITION ----------------------------------------------------------

module Horrocubes.Counter
(
  counterScript,
  counterScriptShortBs,
  CounterParameter(..),
  CounterDatum(..)
) where

-- IMPORTS --------------------------------------------------------------------

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import qualified PlutusTx
import           PlutusTx.Prelude         as P hiding (Semigroup (..), unless)
import           Data.Aeson               (FromJSON, ToJSON)
import           GHC.Generics             (Generic)
import qualified Ledger.Contexts          as Validation
import           Text.Show
import           PlutusTx.Builtins

-- DATA TYPES -----------------------------------------------------------------

-- | The parameters for the counter contract.
data CounterParameter = CounterParameter {
        cpOwnerPkh    :: !PubKeyHash, -- ^ The transaction that spends this output must be signed by the private key
        cpIdentityNft :: !AssetClass  -- ^ The NFT that identifies the correct eUTXO.
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''CounterParameter

-- | The counter datum datatype.
data CounterDatum = CounterDatum {
        cdValue :: !Integer, -- ^ The current counter value.
        cdLimit :: !Integer  -- ^ The value limit, after this limit is reached, this eUTXO can not be spent again.
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''CounterDatum

-- | The Counter script type. Sets the Redeemer and Datum types for this script.
data Counter 
instance Scripts.ValidatorTypes Counter where
    type instance DatumType Counter = CounterDatum
    type instance RedeemerType Counter = ()
    
-- DEFINITIONS ----------------------------------------------------------------

-- | Maybe gets the datum from the transatcion output.
{-# INLINABLE counterDatum #-}
counterDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe CounterDatum
counterDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

-- | Checks that the identity NFT is locked again in the contract.
{-# INLINABLE isIdentityNftRelocked #-}
isIdentityNftRelocked:: CounterParameter -> Value -> Bool
isIdentityNftRelocked params valueLockedByScript = assetClassValueOf valueLockedByScript (cpIdentityNft params) == 1

-- | Creates the validator script for the outputs on this contract.
{-# INLINABLE mkCounterValidator #-}
mkCounterValidator :: CounterParameter -> CounterDatum -> () -> ScriptContext -> Bool
mkCounterValidator parameters oldDatum _ ctx = 
    let isRightNexCounterValue = (newDatumIntegerValue == (oldDatumIntegerValue + 1))
        isIdentityLocked       = isIdentityNftRelocked parameters valueLockedByScript
        isLimitTheSame         = oldDatumLimitValue == newDatumLimitValue
        isLimitNotReached      = newDatumIntegerValue < newDatumLimitValue
    in traceIfFalse "Wrong counter value"           isRightNexCounterValue && 
       traceIfFalse "Identity NFT missing"          isIdentityLocked && 
       traceIfFalse "Missing signature"             isTransactionSignedByOwner &&
       traceIfFalse "Limit value changed"           isLimitTheSame &&
       traceIfFalse "Limit reached"                 isLimitNotReached
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        ownOutput :: TxOut
        ownOutput = case getContinuingOutputs ctx of
            [o] -> o
            _   -> traceError "Expected exactly one output"

        newDatum :: CounterDatum
        newDatum = case counterDatum ownOutput (`findDatum` info) of
            Nothing -> traceError "Counter output datum not found"
            Just datum  -> datum

        oldDatumIntegerValue :: Integer
        oldDatumIntegerValue = cdValue oldDatum

        oldDatumLimitValue :: Integer
        oldDatumLimitValue = cdLimit oldDatum

        newDatumIntegerValue :: Integer
        newDatumIntegerValue = cdValue newDatum

        newDatumLimitValue :: Integer
        newDatumLimitValue = cdLimit newDatum

        valueLockedByScript :: Value
        valueLockedByScript = Validation.valueLockedBy info (Validation.ownHash ctx)

        isTransactionSignedByOwner :: Bool
        isTransactionSignedByOwner = txSignedBy info (cpOwnerPkh parameters)

-- | The script instance of the counter. It contains the mkCounterValidator function
--   compiled to a Plutus core validator script.
counterInstance :: CounterParameter -> Scripts.TypedValidator Counter
counterInstance counter = Scripts.mkTypedValidator @Counter
    ($$(PlutusTx.compile [|| mkCounterValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode counter) $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @CounterDatum @()

-- | Gets the counter validator script that matches the given parameters.
counterValidator :: CounterParameter -> Validator
counterValidator params = Scripts.validatorScript . counterInstance $ params

-- | Generates the plutus script.
counterPlutusScript :: CounterParameter -> Script
counterPlutusScript params = unValidatorScript $ counterValidator params

-- | Serializes the contract in CBOR format.
counterScriptShortBs :: CounterParameter -> SBS.ShortByteString
counterScriptShortBs params = SBS.toShort . LBS.toStrict $ serialise $ counterPlutusScript params

-- | Gets a serizlized plutus script from the given parameters.
counterScript :: PubKeyHash -> AssetClass -> PlutusScript PlutusScriptV1
counterScript pkh ac = PlutusScriptSerialised $ counterScriptShortBs $ CounterParameter { cpOwnerPkh = pkh,  cpIdentityNft = ac }
