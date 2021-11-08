{-|
Module      : plutus-horrocubes-counter.
Description : Application to generate the plutus script of the counter contract.
License     : Apache-2.0
Maintainer  : angel.castillob@protonmail.com
Stability   : experimental
-}

-- LANGUAGE EXTENSIONS --------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

-- IMPORTS --------------------------------------------------------------------

import Cardano.Api                     hiding (TxId)
import Data.String                     (IsString (..))
import Ledger
import Ledger.Bytes                    (getLedgerBytes)
import Prelude
import System.Environment              (getArgs)
import Data.Hex
import qualified Plutus.V1.Ledger.Api  as Plutus
import qualified Data.ByteString.Short as SBS
import           Ledger.Value          as Value
import Horrocubes.Counter
import qualified PlutusTx
import qualified Data.ByteString.Lazy  as LBS
import           Data.Aeson            (encode)
import qualified Plutus.V1.Ledger.Api as Plutus
import           Plutus.V1.Ledger.Contexts
import qualified Cardano.Api              as Script
import qualified Cardano.Api.Shelley      as Script
import qualified Data.ByteString.Lazy.Char8 as C

-- DEFINITIONS ----------------------------------------------------------------

-- | Application entry point.
-- The user must provide four arguments: The currency symbol, the token name, the publick key hash and the output path.
main :: IO ()
main = do
    [currencySymbol', tokenName', publicKey', filePath] <- getArgs
    let nft       = Value.AssetClass ( toCurrencySymbol currencySymbol', toTokenName tokenName')
        publicKey = toPublicKeyHash publicKey'

    counterContractResult <- writeFileTextEnvelope filePath Nothing $ counterScript publicKey nft
    case counterContractResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote NFT policy to file " ++ filePath

    writePlutusScript $ counterScriptShortBs (CounterParameter {cpIdentityNft = nft, cpOwnerPkh = publicKey})
    putStrLn $ show $ toCurrencySymbol currencySymbol'
    putStrLn $ show $ nft
    putStrLn $ datumJSON (CounterDatum {cdValue = 0, cdLimit = 3})

-- | Displays the execution budget.
writePlutusScript :: SBS.ShortByteString -> IO ()
writePlutusScript scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS []
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"

-- | Creates a token name from a byte array.
toCurrencySymbol :: String -> CurrencySymbol
toCurrencySymbol symbol = CurrencySymbol { unCurrencySymbol = getLedgerBytes $ fromString $ symbol }

-- | Creates a token name from a byte array.
toTokenName :: String -> TokenName
toTokenName tn = TokenName { unTokenName = getLedgerBytes $ fromString $ hex tn }

-- | Creates a token name from a byte array.
toPublicKeyHash :: String -> PubKeyHash
toPublicKeyHash pkh = PubKeyHash { getPubKeyHash = getLedgerBytes $ fromString $ pkh }

-- | Parse the UTXO from its hexadecimal string representation to and TxOutRef.
parseUTxO :: String -> TxOutRef
parseUTxO s =
  let
    (x, y) = span (/= '#') s
  in
    TxOutRef (TxId $ getLedgerBytes $ fromString x) $ read $ tail y

datumJSON :: CounterDatum -> String
datumJSON datum = C.unpack $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ Script.fromPlutusData (Plutus.toData datum))

-- Data.Aeson.encode $ scriptDataToJson ScriptDataJsonDetailedSchema $ (fromPlutusData $ builtinDataToData $ toBuiltinData (MyRedeemer ()))
--"{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]}]}"
