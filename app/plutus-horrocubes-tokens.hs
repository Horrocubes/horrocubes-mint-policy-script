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

import Cardano.Api                         hiding (TxId)
import Data.String                         (IsString (..))
import Ledger
import Ledger.Bytes                        (getLedgerBytes)
import Prelude
import System.Environment                  (getArgs)
import Data.Hex
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Data.ByteString.Short    as SBS
import qualified Data.ByteString.Lazy  as LBS
import PlutusTx.Builtins.Class
import PlutusTx.Builtins.Internal
import Data.ByteString hiding (map)
import Data.Char
import Data.Word
import Data.Bits (shift, (.|.))
import qualified Data.ByteString.Char8 as C
import           Data.Char (ord)
import           Data.List (elemIndex)
import Text.Printf
import Horrocubes.MintingScript
import Data.ByteString.Base58
import qualified PlutusTx

-- DEFINITIONS ----------------------------------------------------------------

-- | Application entry point.
-- The user must provide three arguments: UTXO id, the token name and the output path.
main :: IO ()
main = do
    [utxo', tokenName', filePath] <- getArgs
    let utxo      = parseUTxO utxo'
        tokenName = toTokenName tokenName'

    --nftPolicyResult <- writeFileTextEnvelope filePath Nothing $ mintScript utxo tokenName
   -- case nftPolicyResult of
    --    Left err -> print $ displayError err
    --    Right () -> Prelude.putStrLn $ "wrote NFT policy to file " ++ filePath

    --writePlutusScript $ nftScriptShortBs utxo tokenName
    let tka = Plutus.fromBuiltin $ Plutus.unTokenName tokenName
    let tkaEnding = slice 10 20 tka
    let tkposfix = encodeBase $ readInt $ Plutus.fromBuiltin $ parseUTxO utxo'
    Prelude.putStrLn $ show $ tka
    Prelude.putStrLn $ show $ tkaEnding
    Prelude.putStrLn $ show $ (tkaEnding == tkposfix)
    Prelude.putStrLn $ show $ tkposfix
    Prelude.putStrLn $ show $ decodeBase $ encodeBase $ readInt $ Plutus.fromBuiltin $ parseUTxO utxo'    
    --Prelude.putStrLn $ show $ encodeBase $ parseHex $ fromString $ utxo'
    --Prelude.putStrLn $ show $ decodeBase $ encodeBase $ parseHex $ fromString $ utxo'
    Prelude.putStrLn $ printf "The value of %d in hex is: 0x%08x" (getInt $ decodeBase $ encodeBase $ readInt $ Plutus.fromBuiltin $ parseUTxO utxo'  ) (getInt $ decodeBase $ encodeBase $ readInt $ Plutus.fromBuiltin $ parseUTxO utxo'  )


slice :: Int -> Int -> ByteString -> ByteString
slice start len = C.take len . C.drop start

getInt :: Maybe Integer -> Integer
getInt (Just x) = x
getInt Nothing = 0

hexChar :: Char -> Integer
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'a' = 10
    | ch == 'b' = 11
    | ch == 'c' = 12
    | ch == 'd' = 13
    | ch == 'e' = 14
    | ch == 'f' = 15
    | otherwise     = 0

parseHex :: String -> Integer
parseHex [] = 0
parseHex hxStr = hexChar (Prelude.last hxStr) + 16 * parseHex (Prelude.init hxStr)

readInt :: ByteString -> Integer
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
        where byte n = fromIntegral $ ord (bs `C.index` n)

-- | Parse the UTXO from its hexadecimal string representation to and TxOutRef.
parseUTxO :: String -> Plutus.BuiltinByteString
parseUTxO s =
  let
    (x, _) = Prelude.span (/= '#') s
  in
    getLedgerBytes $ fromString x

alphabetBase58:: String
alphabetBase58 = "!~}|{zyxwvutsrqponmlkjihgfedcba_^]#[ZYXWVUTSRQPONMLKJIHGFEDCBA@?>=<;:9876543210/.-+*)($&%"

encodeBase :: Integer -> ByteString
encodeBase value = C.pack encoded where
  base     = 89
  encoded  = expand (value `divMod` base) []
  lookup n = alphabetBase58 !! (fromIntegral n)
  expand (dividend, remainder) xs
    | (dividend >  0) = expand (dividend `divMod` base) result
    | (dividend == 0 && remainder >  0) = result
    | (dividend == 0 && remainder == 0) = xs
    where result = [lookup remainder] ++ xs

decodeBase :: ByteString -> Maybe Integer
decodeBase value = decoded where
  (_, decoded)   = Prelude.foldr reducer accumulator chars
  chars          = (C.unpack value)
  base           = 89
  shift exponent = (* (base ^ exponent))
  lookup char    = toInteger <$> Data.List.elemIndex char alphabetBase58
  accumulator    = (0, Just 0)
  reducer (char) (exponent, amount) =
    (exponent + 1, (+) <$> amount <*> (shift exponent <$> lookup char))

-- | Creates a token name from a byte array.
toTokenName :: String -> Plutus.TokenName
toTokenName tn = Plutus.TokenName { Plutus.unTokenName = getLedgerBytes $ fromString $ hex tn }

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