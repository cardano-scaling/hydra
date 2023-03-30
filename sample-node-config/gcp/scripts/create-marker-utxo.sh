#!/usr/bin/env bash
# Create a 'fuel' UTXO for spending into a Hydra Head
# Assume NETWORK environment variable is set. It should be either `--mainnet` or `--testnet-magic MAGIC`

set -evx

[ $# -eq 1 ] || { echo "need signing key file" ; exit 1 ; }

[ -z "${NETWORK}" ] && { echo "no NETWORK defined" ; exit 2 ; }

ccli () {
        docker run -it -v$(pwd):/work -w /work -u $(id -u) -e CARDANO_NODE_SOCKET_PATH=/work/ipc/node.socket --entrypoint cardano-cli inputoutput/cardano-node:latest ${@}
}

# Some random sink address to send the change to.
# DO NOT CHANGE.
sink=addr_test1vpvt8svlmq4xqjmr3jr6pcq5tg6cc0ry46wtkmd7tjgzynsskex8u

sk=$1

[ $sk ] || (echo "Missing argument: secret key file." && exit 1)

vk=temp.vkey

ccli key verification-key \
    --signing-key-file $sk \
    --verification-key-file $vk

addr=$(ccli address build ${NETWORK} --payment-verification-key-file $vk)

utxo=temp.utxo
ccli query utxo \
    --cardano-mode --epoch-slots 21600 \
    ${NETWORK} \
    --address $addr \
    --out-file $utxo.out
jq "to_entries|sort_by(.value.value.lovelace)|last" $utxo.out > $utxo
input=$(jq '.key' $utxo | tr -d '"')
fuel_amount=$(jq '.value.value.lovelace - 10000000' $utxo)

tx=temp.tx

ccli transaction build \
    --alonzo-era \
    --cardano-mode --epoch-slots 21600 \
    ${NETWORK} \
    --script-valid \
    --tx-in $input \
    --tx-out $addr+$fuel_amount \
    --tx-out-datum-hash "a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3" \
    --change-address $sink \
    --out-file $tx

ccli transaction sign \
    --tx-body-file $tx \
    --signing-key-file $sk \
    --out-file $tx.signed

ccli transaction view \
    --tx-file $tx.signed

ccli transaction submit \
    --cardano-mode --epoch-slots 21600 \
    ${NETWORK} \
    --tx-file $tx.signed
