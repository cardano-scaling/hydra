#!/usr/bin/env bash
#
# Fork off some amount of the UTxO owned by given signing key and mark the rest
# as fuel to be used by the hydra-node.
set -e

function usage() {
    echo "Usage: $0 TESTNET KEY LOVELACE"
    exit 1
}

[ ${#} -eq 3 ] || (echo "Wrong number of arguments" && usage)

testnetDir=$(realpath ${1})
[ -d ${testnetDir} ] || (echo "Not a directory TESTNET: ${testnetDir}" && usage)

sk=${2}
[ -f ${sk} ] || (echo "Not a file KEY: ${sk}" && usage)

amount=${3}
[ -n "${amount}" ] || (echo "Missing argument: amoung of LOVELACE" && usage)

magic=$(cat ${testnetDir}/db/protocolMagicId)
export CARDANO_NODE_SOCKET_PATH="${testnetDir}/node.socket"

vk=${sk%.sk}.vk
if [[ ! -f "${vk}" ]]; then
  cardano-cli key verification-key --signing-key-file ${sk} --verification-key-file ${vk}
fi

addr=$(cardano-cli address build --testnet-magic ${magic} --payment-verification-key-file $vk)

utxo=$(cardano-cli query utxo \
    --cardano-mode --epoch-slots 21600 \
    --testnet-magic ${magic} \
    --address ${addr} \
    --out-file /dev/stdout)
totalLovelace=$(echo ${utxo} | jq -r 'reduce .[] as $item (0; . + $item.value.lovelace)')
[ ${totalLovelace} -eq 0 ] && echo "Error: insufficient funds" && exit 1

entries=$(echo ${utxo} | jq "to_entries|sort_by(.value.value.lovelace)|last")
input=$(echo ${entries} | jq '.key' | tr -d '"')
fuelAmount=$(echo ${entries} | jq ".value.value.lovelace - ${amount}")

tx=$(mktemp)
cardano-cli transaction build \
    --babbage-era \
    --cardano-mode --epoch-slots 21600 \
    --testnet-magic ${magic} \
    --script-valid \
    --tx-in ${input} \
    --tx-out ${addr}+${fuelAmount} \
    --tx-out-datum-hash "a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3" \
    --change-address ${addr} \
    --out-file ${tx}

cardano-cli transaction sign \
    --tx-body-file ${tx} \
    --signing-key-file $sk \
    --out-file ${tx}.signed

echo "-----------------------------------------------"
echo "Prepared transaction in ${tx}.signed, for details use:"
echo "cardano-cli transaction view --tx-file ${tx}.signed"

echo "-----------------------------------------------"
read -p "Submit transaction now? Y/n" -n 1 -r
echo
echo "-----------------------------------------------"
if [[ $REPLY =~ ^[Yy]$ ]]; then
    cardano-cli transaction submit \
        --cardano-mode \
        --epoch-slots 21600 \
        --testnet-magic ${magic} \
        --tx-file ${tx}.signed
    rm ${tx}.signed
fi
