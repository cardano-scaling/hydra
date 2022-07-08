#!/usr/bin/env bash

set -e

# magic=9 # vasil testnet
magic=1097911063 # testnet

sk=$1
[ -f $sk ] || (echo "Missing argument: secret key file" && exit 1)

amount=$2
[ -n "${amount}" ] || (echo "Missing argument: amount" && exit 1)

vk=${sk%.sk}.vk
if [[ ! -f "${vk}" ]]; then
  cardano-cli key verification-key --signing-key-file ${sk} --verification-key-file ${vk}
fi

addr=$(cardano-cli address build --testnet-magic ${magic} --payment-verification-key-file $vk)

utxo=$(mktemp)
cardano-cli query utxo \
    --cardano-mode --epoch-slots 21600 \
    --testnet-magic ${magic} \
    --address $addr \
    --out-file /dev/stdout | jq "to_entries|sort_by(.value.value.lovelace)|last" > $utxo.json
input=$(jq '.key' $utxo.json | tr -d '"')
fuel_amount=$(jq ".value.value.lovelace - ${amount}" $utxo.json)

tx=$(mktemp)
cardano-cli transaction build \
    --babbage-era \
    --cardano-mode --epoch-slots 21600 \
    --testnet-magic ${magic} \
    --script-valid \
    --tx-in $input \
    --tx-out $addr+$fuel_amount \
    --tx-out-datum-hash "a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3" \
    --change-address ${addr} \
    --out-file $tx

cardano-cli transaction sign \
    --tx-body-file $tx \
    --signing-key-file $sk \
    --out-file $tx.signed


echo "----------------------------------------------"
echo "Prepared transaction: $tx.signed"

cardano-cli transaction view \
    --tx-file $tx.signed

echo "----------------------------------------------"
echo "To submit:"
echo "cardano-cli transaction submit --cardano-mode --epoch-slots 21600 --testnet-magic ${magic} --tx-file $tx.signed"
