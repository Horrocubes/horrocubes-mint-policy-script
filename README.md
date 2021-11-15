<p align="center">
  <img align="middle" src=
  "https://github.com/Horrocubes/horrocubes-mint-policy-script/blob/main/assets/horrologo_black.png"
  height="250" /></br>
  <sup><sup><sup><sup>The Horrocubes logo is licensed under
  <a href="https://creativecommons.org/licenses/by/3.0/">Creative
  Commons 3.0 Attributions license</a></sup></sup></sup></sup>
</p>
 
 ![license](https://img.shields.io/badge/license-APACHE-blue.svg?longCache=true&style=flat) 

<p align="center"><b>Horrocubes Minting Policy Script</b></p>

Horrocubes NFTs leverage the power of Plutus minting policy scripts to create true NFTs; once these NFTs are minted, it is impossible to create a duplicate. This minting policy makes use of several contracts to achieve the atomic creation of immutable NFTs while keeping the same policy id. 

If your Horrocube is postfixed with a hexadecimal value (e.g., Horrocubes00001x0FA488ED), it was created with this policy; otherwise, go over https://github.com/Horrocubes/cardano-nft-factory.

There is a detailed explanation on the following youtube video: https://www.youtube.com/watch?v=vHraLcvECmQ

## Counter Script

The counter script trap an identity NFT inside (the only way to spend the output is to pay the NFT back to the script) and force the increment of the internal counter until it reaches a limit (after that, the output becomes unspendable), the starting value of the counter and the limit are defined when the output is first created.

#### Pre-conditions:

 - The transaction spending this output must be signed with the proper key (passed as a script parameter)
 - The identity NFT must be present (this is indirectly validated by the fact that the script check that the identity NFT is being paid to itself)

#### Post-Conditions:

 - The field cdValue of the new datum must be equals to the field cdValue of the old datum plus 1 (value must be increased by exactly one)
 - The field cdLimit of the new datum must equal to the field cdLimit of the old datum (value cant not be changed between datums)
 - The field cdValue of the new datum must be less than the value of the field cdLimit (once cdValue reach cdLimit, the output can not be spent anymore)
 - The identity NFT must be paid back to the original script.

## MintingScriptWithCounter Script

This script only approves the minting transaction if the identity NFT is present (the same one trapped in the counter eUTXO), the transaction is signed by the proper key, and then takes the datum from the output where the identity NFT is present, encodes it as a 32bit hex string and checks that the asset name of the new token being minted has this value as a postfix (last eight characters must match this hex value).

#### Pre-conditions:

 - The transaction minting the asset must be signed with the proper key (passed as a script parameter)
 - The identity NFT must be present (the script checks that the identity NFT is being spent)
 - The last eight characters of the asset name of the token matches the hexadecimal representation of the counter (in 32bits)

#### Post-Conditions:

 - Only one token with the right postfix value is being minted.

#### Policy-Id:
`160b85e53e25ef49272c421f04b702bc32184d102865fd1dc8815cde`

## Native Tokens

We used ten native tokens to help us identify the correct eUTXO while calculating the right postfix:

| Policy                                     | Policy Id                                                  | Token Name       | Purpose                             |
| ------------------------------------------ | ---------------------------------------------------------- | ---------------- | ----------------------------------- |
| [script](scripts/minterAgents.plutus)      | `cde924ed9494ed6be4ea7d4108588a86cbcf0a9a4ca4d5da19dc7aab` | `HorrocubesMinterAgent`     | Identifies the relevant UTxOs.       |

Ten identity tokens were created to identify the relevant eUTXOs with the counter values, `HorrocubesMinterAgent`. They were minted with the [UtxoMintingScript](src/UtxoMintingScript.hs). This script ties the minting script with the eUTXO that was consumed during minting, which makes it impossible to re-mint the same ten tokens.

All ten tokens were locked in different outputs with different starting and limit values to avoid overlapping, for example:

 - eUTXO 1: starting value 0, limit 1000
 - eUTXO 2: starting value 1000, limit 2000
 - eUTXO 3: starting value 2000, limit 3000

The limit is not inclusive of the upper bound (per the script).

## Initial counter values

These are the initial datum values for all the eUTOX  in the counter contract:

| `Datum Value`                                                    | `Datum Hash` | 
| -----------------------------------------------------------------| ---------------| 
|{"constructor":0,"fields":[{"int":0},{"int":429496729}]}          |`5d84fbf618fe74a270cc01b16023649905b965b8cc2563d3500dcdfa66108725`| 
|{"constructor":0,"fields":[{"int":429496730},{"int":858993458}]}  |`80fda78cdb4a79fb622a2e60107a6f62a8fa1f8799024ac07292404b76df1794`| 
|{"constructor":0,"fields":[{"int":858993459},{"int":1288490187}]} |`91217e85ef60f44186996cbae95a91ca213408b143608c631d0b6549eecb506c`| 
|{"constructor":0,"fields":[{"int":1288490188},{"int":1717986916}]}|`2c8094e31625fe2c45d63fdba8beacc96629ed1474b55ebb67011d29fd33f950`| 
|{"constructor":0,"fields":[{"int":1717986917},{"int":2147483645}]}|`e46d61b8a2d36680f4f50664587b3f1da5c03a8e20fb73cb25cb3aa760852947`| 
|{"constructor":0,"fields":[{"int":2147483646},{"int":2576980374}]}|`0065c0f205a67d812180c05e4e484417aa5fdb3001d3a3872ef590abce5fdfb3`|
|{"constructor":0,"fields":[{"int":2576980375},{"int":3006477103}]}|`655210e87dd93a7dc3840b13f1f6843ffeb61bcf2335cf41f53f8e7cafa16290`| 
|{"constructor":0,"fields":[{"int":3006477104},{"int":3435973832}]}|`d04ee8b51c64892c41455a2a6d8c6bd9739489c088e33bb441812477e7f566b3`| 
|{"constructor":0,"fields":[{"int":3435973836},{"int":3865470561}]}|`dabbff379296be4257e9d8bdeb1097ed7d8018e783895951733d9f620b3b759d`| 
|{"constructor":0,"fields":[{"int":3865470562},{"int":4294967295}]}|`29dbba2ed5761d4b7f2e167399ec758fc5f088928e459edd87440e6fad680b8a`| 

All the counter outputs are located at the script address:

`addr1wye3sfyn0h2yyh65dyzcqkrjnxtl3lk6qgv9g2n6aa3pg0qfjmdan`

## Code

You can look at the Plutus code [here](src/Horrocubes),
where you will find the minting policies for the used native tokens and the validator enforcing the contract logic.

You can build the code with `cabal build`.
