#!/usr/bin/env bash
# a script for testing local cardano-node cluster

set -vx

[ -x $(which cardano-cli) ] || { echo "cardano-cli not found, check PATH environment variable" ; exit 1 ; }

utxo_addr=$1
transfer_amount=100000000

timeout=30

while [ $timeout -gt 0 ]; do
  new_utxo=$(cardano-cli query utxo --testnet-magic 42 --address $alice_addr | grep $transfer_amount)

  [ ! -z "$new_utxo" ] && { echo "found UTXO: $new_utxo" ; exit 0 ; }

  echo "new UTXO not found, waiting 1s"
  sleep 1
  timeout=$(( $timeout - 1 ))
done

exit 1
