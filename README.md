# horrocubes-mint-policy-script
New policy validator script for the horrocubes project

# {"constructor":0,"fields":[{"int":0}]}  cb61a2430a1eef83214bc9b7c36e2d7df5edf0b2f0b00779a54e6603fcbc74af

# Step 1 Init contract

# Create Address
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli address build --payment-verification-key-file counter/policy.vkey  --testnet-magic 1097911063

# Query UTXO
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli query utxo --address addr_test1vpfvmwfl8eucm8rnsej9pehzh7628k53raczagz4uvzzm2csx7sfl --testnet-magic 1097911063

# Create Script Address
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli address build --payment-script-file ./counter/out2.plutus --testnet-magic 1097911063

# Create Datum Hash
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction hash-script-data --script-data-value 0


# Create Build transaction
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction build --alonzo-era --testnet-magic 1097911063 --change-address $(cat counter/payment.addr) --tx-in ab2559b4fbf6a886f398e21942528ff94f40dac8ce80fcd236b99571384fd8a1#0 --tx-out "addr_test1wpuf4g5hwdw9sm0872jju6a0lu2cuckat8fkrhv6klsncggckm2c0+2000000" --tx-out-datum-embed-file ./datum_0.json --protocol-params-file protocol.json --out-file tx-script.build

# Query UTXO
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli query utxo --address addr_test1wp8m5q4lhpur7sn7mr5qwzzt7syxksk4u0pq82euef5t5csx2ya6u --testnet-magic 1097911063

# Sign the transaction 
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction sign --tx-body-file tx-script.build --signing-key-file counter/policy.skey --testnet-magic 1097911063 --out-file tx-script.signed

# Send Transaction
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction submit --testnet-magic 1097911063 --tx-file tx-script.signed

# TX HASH 1 (init 0 value)
b2997baf426caa94762e4baeed051ac13bad7994f2f3a43f8c43299d2ba8f050

# Step 2 increment counter

# Create Datum Hash (value 1)
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction hash-script-data --script-data-value 1

ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25


# Create Build transaction
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction build-raw --alonzo-era --fee 500000 --tx-in 1215cc8fef96f600f0ef7a9b0250165c2167cb9c48ac0b7e07afe40e60675bdd#0 --tx-in 4d409145e8b4ff2ba76154c3caeb76cba8f4f0645e74f8f1eb18222dfe863935#1 --tx-in-script-file ./counter/out2.plutus --tx-in-execution-units "(491845099,1197950)" --tx-in-datum-value 0 --tx-in-redeemer-value 0 --tx-in-collateral 1215cc8fef96f600f0ef7a9b0250165c2167cb9c48ac0b7e07afe40e60675bdd#0 --tx-out "addr_test1wr8mnwgavh0es25kgmhjzylga38drknmmhzszla7y6m9nfqdk8zp4+2000000+1 5dafba002ad1625fdc44b5ae5eb1331ca8fb774415503952846333fe.HorrocardTheFoolHalloween2100018" --tx-out-datum-embed-value 1 --protocol-params-file protocol.json --out-file tx-script2.build

# Sign the transaction 
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction sign --tx-body-file tx-script2.build --signing-key-file counter/policy.skey --testnet-magic 1097911063 --out-file tx-script2.signed

# Send Transaction
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction submit --testnet-magic 1097911063 --tx-file tx-script2.signed




docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction build-raw --alonzo-era --fee 500000 --tx-in 9a641585c53e8ff8032730c5d4677c20c5dc02deb09dc78d1f5651a93852df62#1 --tx-in-script-file ./counter/out2.plutus --tx-in-execution-units "(491845099,1197950)" --tx-in-datum-file ./datum_0.json --tx-in-redeemer-value [] --tx-in-collateral 185ae3f424a1e5551d1c5ae77bd4fe271d321b936952c4e12aebf8f07b7dcf54#0 --tx-out "addr_test1vpfvmwfl8eucm8rnsej9pehzh7628k53raczagz4uvzzm2csx7sfl+1000000" --tx-out-datum-embed-file ./datum_1.json --protocol-params-file protocol.json --out-file tx-script2.build

./datum_0.json




docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction build --alonzo-era --testnet-magic 1097911063  --change-address $(cat counter/payment.addr) --tx-in-collateral 7d65ca46aca44532d94da57ec6b7297efdda2e523626f90bbdc780f3767202f2#0 --tx-in e2cd5091d4077f9d1011d1b5268eff1e408282930e4e346f007f4aee6665124b#1 --tx-in-script-file ./counter/out2.plutus --tx-in-datum-file ./datum_0.json --tx-in-redeemer-value [] --tx-out "addr_test1vpfvmwfl8eucm8rnsej9pehzh7628k53raczagz4uvzzm2csx7sfl+1000000" --tx-out-datum-embed-file ./datum_1.json  --protocol-params-file protocol.json --out-file tx-script2.build



docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction build --alonzo-era --testnet-magic 1097911063  --change-address $(cat counter/payment.addr) --tx-in-collateral 7d65ca46aca44532d94da57ec6b7297efdda2e523626f90bbdc780f3767202f2#0 --tx-in e2cd5091d4077f9d1011d1b5268eff1e408282930e4e346f007f4aee6665124b#1 --tx-in-script-file ./counter/out2.plutus --tx-in-datum-file ./datum_0.json --tx-in-redeemer-file ./datum_0.json --tx-out "addr_test1wpuf4g5hwdw9sm0872jju6a0lu2cuckat8fkrhv6klsncggckm2c0+1000000" --tx-out-datum-embed-file ./datum_1.json  --protocol-params-file protocol.json --out-file tx-script2.build





docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction build --alonzo-era --testnet-magic 1097911063  --change-address $(cat counter/payment.addr) --tx-in-collateral 7d65ca46aca44532d94da57ec6b7297efdda2e523626f90bbdc780f3767202f2#0 --tx-in 18013ebf67123b4259fb1cc469ad38b63e181de2c7eb5c755eb498792dda0fde#1 --tx-in-script-file ./counter/out2.plutus --tx-in-datum-file ./datum_0.json --tx-in-redeemer-file ./datum_0.json --tx-out "addr_test1wpuf4g5hwdw9sm0872jju6a0lu2cuckat8fkrhv6klsncggckm2c0+1000000" --tx-out-datum-embed-file ./datum_1.json  --protocol-params-file protocol.json --out-file tx-script2.build





docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction build-raw  --alonzo-era --fee 500000 --tx-in-collateral 7d65ca46aca44532d94da57ec6b7297efdda2e523626f90bbdc780f3767202f2#0 --tx-in e2cd5091d4077f9d1011d1b5268eff1e408282930e4e346f007f4aee6665124b#1 --tx-in-script-file ./counter/out2.plutus --tx-in-execution-units "(491845099,1197950)" --tx-in-datum-file ./datum_0.json --tx-in-redeemer-file ./datum_0.json --tx-out "addr_test1wpuf4g5hwdw9sm0872jju6a0lu2cuckat8fkrhv6klsncggckm2c0+2000000" --tx-out-datum-embed-file ./datum_1.json  --protocol-params-file protocol.json --out-file tx-script2.build


docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction build-raw  --alonzo-era --fee 500000 --tx-in 7d65ca46aca44532d94da57ec6b7297efdda2e523626f90bbdc780f3767202f2#0 --tx-in-collateral 7d65ca46aca44532d94da57ec6b7297efdda2e523626f90bbdc780f3767202f2#0 --tx-in e2cd5091d4077f9d1011d1b5268eff1e408282930e4e346f007f4aee6665124b#1 --tx-in-script-file ./counter/out2.plutus --tx-in-execution-units "(491845099,1297950)" --tx-in-datum-file ./datum_0.json --tx-in-redeemer-file ./datum_0.json --tx-out "addr_test1wpuf4g5hwdw9sm0872jju6a0lu2cuckat8fkrhv6klsncggckm2c0+2000000" --tx-out-datum-embed-file ./datum_1.json  --tx-out "addr_test1vpfvmwfl8eucm8rnsej9pehzh7628k53raczagz4uvzzm2csx7sfl+18159890"  --protocol-params-file protocol.json --out-file tx-script2.build



docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction build --alonzo-era --testnet-magic 1097911063  --change-address $(cat counter/payment.addr) --tx-in 9a641585c53e8ff8032730c5d4677c20c5dc02deb09dc78d1f5651a93852df62#0 --tx-in-collateral 9a641585c53e8ff8032730c5d4677c20c5dc02deb09dc78d1f5651a93852df62#0 --tx-in 18013ebf67123b4259fb1cc469ad38b63e181de2c7eb5c755eb498792dda0fde#1 --tx-in-script-file ./counter/out2.plutus --tx-in-datum-file ./datum_0.json --tx-in-redeemer-file ./datum_0.json --tx-out "addr_test1wpuf4g5hwdw9sm0872jju6a0lu2cuckat8fkrhv6klsncggckm2c0+2000000" --tx-out-datum-embed-file ./datum_1.json  --protocol-params-file protocol.json --out-file tx-script2.build





2000000 + 18659890