#!/usr/bin/env bash
# An extremely crude script to submit inittx then aborttx through cardano-cli

set -evx

[ -x cardano-cli ] || { echo "cardano-cli not found" ; exit 1 ; }

alice_addr=$(cat alice/payment.addr)
utxo_addr=$(cardano-cli address build --payment-verification-key-file example/shelley/utxo-keys/utxo1.vkey     --testnet-magic 42)

# take first utxo available
utxo=$(cardano-cli query utxo --testnet-magic 42 --address $utxo_addr | tail -n +3 | tr -s ' ' | cut -d ' ' -f1,2 | tr ' ' '#')
amount=$(cardano-cli query utxo --testnet-magic 42 --address $utxo_addr | tail -n +3 | tr -s ' ' | cut -d ' ' -f3)

# dispatch some ADAs to alice
cardano-cli transaction build-raw --tx-in $utxo \
            --tx-out $(cat alice/payment.addr)+100000000 \
            --tx-out $(cat alice/payment.addr)+100000000 \
            --tx-out $(cat alice/payment.addr)+100000000 --invalid-hereafter 0 --fee 0 --out-file tx.draft

fees=$(cardano-cli transaction calculate-min-fee --tx-body-file tx.draft --tx-in-count 1 --tx-out-count 4 --witness-count 1 --testnet-magic 42 --genesis example/shelley/genesis.json | cut -d ' ' -f1)
slot=$(cardano-cli query tip --testnet-magic 42 | jq .slot)

cardano-cli transaction build-raw --tx-in $utxo \
            --tx-out $(cat alice/payment.addr)+100000000 \
            --tx-out $(cat alice/payment.addr)+100000000 \
            --tx-out $(cat alice/payment.addr)+100000000 \
            --tx-out $utxo_addr+$(($amount - 100000000 - 100000000 - 100000000 - $fees)) \
            --invalid-hereafter $((slot + 100)) --fee $fees --out-file tx.draft

cardano-cli transaction sign --tx-body-file tx.draft --signing-key-file example/shelley/utxo-keys/utxo1.skey --testnet-magic 42 --out-file tx.signed
cardano-cli transaction submit --tx-file tx.signed --testnet-magic 42

sleep 2

# head init transaction
init_utxo=$(cardano-cli query utxo --testnet-magic 42 --address $alice_addr | tail -n +3 | head -1 | tr -s ' ' | cut -d ' ' -f1,2 | tr ' ' '#')
head_address=$(cardano-cli address build --payment-script-file headScript.plutus --testnet-magic 42)
cardano-cli transaction build --alonzo-era --cardano-mode --testnet-magic 42 \
            --change-address $(cat alice/payment.addr) \
            --tx-in $init_utxo \
            --tx-out $head_address+1000 \
            --tx-out-datum-hash 50fb0e30e734c617394ca1f230b20d6971b945dfc1c25df6257702c5f307789d --out-file head.draft
cardano-cli transaction sign --tx-body-file head.draft --signing-key-file alice/payment.skey --testnet-magic 42 --out-file head.signed
cardano-cli transaction submit --tx-file head.signed --testnet-magic 42

sleep 2

# head abort transaction
cardano-cli query protocol-parameters --testnet-magic 42 --out-file example/pparams.json

# UTXO to pay for the script's execution and collateral
pay_utxo=$(cardano-cli query utxo --testnet-magic 42 --address $alice_addr | grep 100000000 | head -1 | tr -s ' ' | cut -d ' ' -f1,2 | tr ' ' '#')
init_utxo=$(cardano-cli query utxo --testnet-magic 42 --address $head_address | grep 50fb0e30e734c617394ca1f230b20d6971b945dfc1c25df6257702c5f307789d | head -1 | tr -s ' ' | cut -d ' ' -f1,2 | tr ' ' '#')

cardano-cli transaction build --alonzo-era --cardano-mode --testnet-magic 42 \
            --change-address $alice_addr \
            --tx-in $pay_utxo \
            --tx-in $init_utxo \
            --tx-in-script-file headScript.plutus \
            --tx-in-datum-file headDatum.data \
            --tx-in-redeemer-file headRedeemer.data \
            --tx-in-collateral $pay_utxo \
            --tx-out $head_address+1000 \
            --tx-out-datum-embed-file abortDatum.data \
            --out-file abort.draft --protocol-params-file example/pparams.json
cardano-cli transaction sign --tx-body-file abort.draft --signing-key-file alice/payment.skey --testnet-magic 42 --out-file abort.signed
cardano-cli transaction submit --tx-file abort.signed --testnet-magic 42
