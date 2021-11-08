# horrocubes-mint-policy-script
New policy validator script for the horrocubes project

# {"constructor":0,"fields":[{"int":0}]}  cb61a2430a1eef83214bc9b7c36e2d7df5edf0b2f0b00779a54e6603fcbc74af

# Step 1 Init contract

# Create Address
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli address build --payment-verification-key-file counter/policy.vkey  --testnet-magic 1097911063

# Query UTXO
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli query utxo --address addr_test1vpfvmwfl8eucm8rnsej9pehzh7628k53raczagz4uvzzm2csx7sfl --testnet-magic 1097911063

# Create Script Address
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli address build --payment-script-file ./counter/counter.plutus --testnet-magic 1097911063

# Create Datum Hash
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction hash-script-data --script-data-value 0


# Create Build transaction
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction build --alonzo-era --testnet-magic 1097911063 --change-address $(cat counter/payment.addr) --tx-in 400298971545e4abcebe89ca6a15f3cd6754056ce7744e49886f26a7dea0cf75#0 --tx-in 62aedb16d26984e34e6871494ba542b2f40a960c5a0838226e2c0244ce056da2#0 --tx-out "addr_test1wz6g45e97dkqs3zxfptcmqt7lrssjjxg8aa389y5xyuwj3swtmgve+2000000+1 06fa00b5ae593280e3d3a6693688523c85af4ff3033ddc8794ae311a.Horrocube00028" --tx-out-datum-embed-file ./datum_0.json --protocol-params-file protocol.json --out-file tx-script.build

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
docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction build --alonzo-era --testnet-magic 1097911063  --change-address $(cat counter/payment.addr) --tx-in 400298971545e4abcebe89ca6a15f3cd6754056ce7744e49886f26a7dea0cf75#0 --tx-in-collateral 400298971545e4abcebe89ca6a15f3cd6754056ce7744e49886f26a7dea0cf75#0 --tx-in 62aedb16d26984e34e6871494ba542b2f40a960c5a0838226e2c0244ce056da2#1 --tx-in-script-file ./counter/counter.plutus --tx-in-datum-file ./datum_0.json --tx-in-redeemer-file ./datum_0.json --tx-out "addr_test1wz6g45e97dkqs3zxfptcmqt7lrssjjxg8aa389y5xyuwj3swtmgve+2000000+1 ba86415058081a628b54d29953d3f6dae2480ea9d9e2cc852a8ea449.HCube097043XEdfDgC4VJUq2ut1X7Fb4" --tx-out-datum-embed-file ./datum_0.json --required-signer counter/policy.skey --protocol-params-file protocol.json --out-file tx-script2.build

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



docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction build --alonzo-era --testnet-magic 1097911063  --change-address $(cat counter/payment.addr) --tx-in cb9dec86b66b82613f88b50f16a63e12701d6775c5f7055eaf2c19c2cb82dcf5#0 --tx-in-collateral cb9dec86b66b82613f88b50f16a63e12701d6775c5f7055eaf2c19c2cb82dcf5#0 --tx-in ab55a4351a5004dd3a8f50d22a782449af02c10cd68c40907eeb49cf0a3e4aa4#1 --tx-in-script-file ./counter/out2.plutus --tx-in-datum-file ./datum_0.json --tx-in-redeemer-file ./datum_0.json --tx-out "addr_test1wp9l0xxmvvvxsfhszwsf6t7f2ghvdf4r95hq3xjezzp4vgccftayp+2000000+1 38a2383fc478349ea5dd47c9bcb19591fe9ed700457b28d56ba3515c.HorrocardTheFoolHalloween2100017" --tx-out-datum-embed-file ./datum_1.json --required-signer counter/policy.vkey --protocol-params-file protocol.json --out-file tx-script2.build





2000000 + 18659890












@001df3c9





docker exec -w /work -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket docker_cardano-node_1 cardano-cli transaction build --alonzo-era --testnet-magic 1097911063  --change-address $(cat counter/payment.addr) --tx-in 579a7343ccb3358e6d0840ede41428e49d52db0aaa25a20e52b90ed3d58f02f9#0 --tx-in-collateral 579a7343ccb3358e6d0840ede41428e49d52db0aaa25a20e52b90ed3d58f02f9#0 --tx-in  579a7343ccb3358e6d0840ede41428e49d52db0aaa25a20e52b90ed3d58f02f9#1 --tx-in-script-file ./counter/counter.plutus --tx-in-datum-file ./datum_0.json --tx-in-redeemer-file ./datum_0.json --tx-out "addr_test1wz6g45e97dkqs3zxfptcmqt7lrssjjxg8aa389y5xyuwj3swtmgve+2000000+1 06fa00b5ae593280e3d3a6693688523c85af4ff3033ddc8794ae311a.Horrocube00028" --tx-out-datum-embed-file ./datum_1.json --required-signer counter/policy.skey --tx-out "addr_test1vpfvmwfl8eucm8rnsej9pehzh7628k53raczagz4uvzzm2csx7sfl+1413762+1 8bc230df616dedc8f35f61998a76c22bc516817d531c22d8b5025653.Horrocube00279c00000001" --mint "1 8bc230df616dedc8f35f61998a76c22bc516817d531c22d8b5025653.Horrocube00279c00000001" --minting-script-file ./counter/mint.plutus --mint-redeemer-value [] --required-signer counter/policy.skey --protocol-params-file protocol.json --json-metadata-no-schema --metadata-json-file 5de524fd-bf08-4794-8847-f7fcb51c9862.metadata --out-file tx-script2.build


