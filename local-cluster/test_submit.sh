#!/usr/bin/env bash
# a script for testing local cardano-node cluster

set -vx

[ -x $(which cardano-cli) ] || { echo "cardano-cli not found, check PATH environment variable" ; exit 1 ; }

utxo_addr=$1
utxo=$2
amount=$3
fees=$4

transfer_amount=100000000

slot=$(cardano-cli query tip --testnet-magic 42 | jq .slot)

cardano-cli transaction build-raw --tx-in $alice_txin \
            --tx-out $alice_addr+$transfer_amount \
            --tx-out $alice_addr+$(($alice_fortune - $transfer_amount - $fees)) \
            --invalid-hereafter $((slot + 100)) --fee $fees --out-file tx.draft

cardano-cli transaction sign --tx-body-file tx.draft --signing-key-file ../alice.sk --testnet-magic 42 --out-file tx.signed
cardano-cli transaction submit --tx-file tx.signed --testnet-magic 42

timeout=30

while [ $timeout -gt 0 ]; do
  new_utxo=$(cardano-cli query utxo --testnet-magic 42 --address $alice_addr | grep $transfer_amount)

  [ ! -z "$new_utxo" ] && { echo "found UTXO: $new_utxo" ; exit 0 ; }

  echo "new UTXO not found, waiting 1s"
  sleep 1
  timeout=$(( $timeout - 1 ))
done

exit 1
