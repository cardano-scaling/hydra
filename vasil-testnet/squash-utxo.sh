#!/usr/bin/env bash
#
# Squash all utxo of a signing key into a single output.

set -e

magic=9 # vasil testnet
# magic=1097911063 # testnet

sk=$1
[[ -n $sk ]] && [[ -f $sk ]] || (echo "Missing argument: secret key file" && exit 1)

vk=${sk%.sk}.vk
if [[ ! -f "${vk}" ]]; then
  cardano-cli key verification-key --signing-key-file ${sk} --verification-key-file ${vk}
fi

addr=$(cardano-cli address build --testnet-magic ${magic} --payment-verification-key-file $vk)

inputs=($(
 cardano-cli query utxo \
    --cardano-mode --epoch-slots 21600 \
    --testnet-magic ${magic} \
    --address $addr \
    --out-file /dev/stdout | jq -r '. | keys | @sh' | tr -d "'"))

echo "Squashing utxos:"
txInArgs=""
for input in ${inputs[@]}; do
    echo " - ${input}"
    txInArgs="${txInArgs}--tx-in ${input} "
done

tx=$(mktemp)
cardano-cli transaction build \
    --babbage-era \
    --cardano-mode --epoch-slots 21600 \
    --testnet-magic ${magic} \
    --script-valid \
    ${txInArgs} \
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
