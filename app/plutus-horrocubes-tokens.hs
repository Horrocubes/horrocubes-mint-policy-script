{-|
Module      : plutus-horrocubes-tokens.
Description : Application to generate NFTs using smart contracts.
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
import Horrocubes.MintingScript
import qualified PlutusTx
import qualified Data.ByteString.Lazy  as LBS
import           Data.Aeson            (encode)
import qualified Plutus.V1.Ledger.Api as Plutus
import           Plutus.V1.Ledger.Contexts
import qualified Cardano.Api              as Script
import qualified Cardano.Api.Shelley      as Script
import qualified Data.ByteString.Lazy.Char8 as C
import Horrocubes.Deserialisation
import           PlutusTx.Builtins 

-- DEFINITIONS ----------------------------------------------------------------

-- | Application entry point.
-- The user must provide three arguments: UTXO id, the token name and the output path.
main :: IO ()
main = do
    [utxo', publicKey', filePath] <- getArgs
    let publicKey = toPublicKeyHash publicKey'
        charset   = toCharset alphabetBase

    counterContractResult <- writeFileTextEnvelope filePath Nothing $ mintScript charset publicKey
    case counterContractResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote NFT policy to file " ++ filePath

    writePlutusScript $ nftScriptShortBs charset publicKey
    putStrLn $ show $ charset
    putStrLn $ show $ publicKey
    putStrLn $ show $ encodeBase charset $ parseUTxO utxo'   

-- | Base charset.
alphabetBase:: String
alphabetBase = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

-- | Creates a token name from a byte array.
toPublicKeyHash :: String -> PubKeyHash
toPublicKeyHash pkh = PubKeyHash { getPubKeyHash = getLedgerBytes $ fromString $ pkh }

-- | Creates the char set as a byte array.
toCharset :: String -> Plutus.BuiltinByteString
toCharset cs = getLedgerBytes $ fromString $ hex cs

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
        Nothing -> Prelude.error "defaultCostModelParams failed"

-- | Encodes an Integer into a diffent base (ie base 64).
encodeBase :: Plutus.BuiltinByteString -> Integer -> Plutus.BuiltinByteString
encodeBase charset value = encoded where
  base     = lengthOfByteString charset
  encoded  = expand (value `divMod` base) emptyByteString
  lookup n = indexByteString charset n
  expand (dividend, rem) xs
    | (dividend >  0) = expand (dividend `divMod` base) result
    | (dividend == 0 && rem >  0) = result
    | (dividend == 0 && rem == 0) = xs
    where result = consByteString (lookup rem) xs

-- | Parse the UTXO from its hexadecimal string representation to and TxOutRef.
parseUTxO :: String -> Integer
parseUTxO s =
  let
    (x, y) = Prelude.span (/= '#') s
  in
    builtinByteStringToInt 0 0 $ sliceByteString 16 16 $ getLedgerBytes $ fromString x
