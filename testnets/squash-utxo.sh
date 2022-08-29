#!/usr/bin/env bash
#
# Squash all utxo of a signing key into a single output and mark it as fuel.
set -e

function usage() {
    echo "Usage: $0 TESTNET KEY"
    exit 1
}

[ ${#} -eq 2 ] || (echo "Wrong number of arguments" && usage)

testnetDir=$(realpath ${1})
[ -d ${testnetDir} ] || (echo "Not a directory TESTNET: ${testnetDir}" && usage)

sk=${2}
[ -f ${sk} ] || (echo "Not a file KEY: ${sk}" && usage)

magic=$(cat ${testnetDir}/db/protocolMagicId)
export CARDANO_NODE_SOCKET_PATH="${testnetDir}/node.socket"

vk=${sk%.sk}.vk
if [[ ! -f "${vk}" ]]; then
  cardano-cli key verification-key --signing-key-file ${sk} --verification-key-file ${vk}
fi

addr=$(cardano-cli address build --testnet-magic ${magic} --payment-verification-key-file $vk)

fuelMarker="a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3"
utxo=$(cardano-cli query utxo \
    --cardano-mode --epoch-slots 21600 \
    --testnet-magic ${magic} \
    --address ${addr} \
    --out-file /dev/stdout)
inputs=($(echo ${utxo} | jq -r '. | keys | @sh' | tr -d "'"))
totalLovelace=$(echo ${utxo} | jq -r 'reduce .[] as $item (0; . + $item.value.lovelace)')

echo "Squashing utxos (total: ${totalLovelace} lovelace):"
txInArgs=""
for input in ${inputs[@]}; do
    echo " - ${input}"
    txInArgs="${txInArgs}--tx-in ${input} "
done

# Use a 2ADA output to have the right number of outputs, but likely enough from
# the inputs to balance
estimatedFee=$(cardano-cli transaction build \
    --babbage-era \
    --cardano-mode --epoch-slots 21600 \
    --testnet-magic ${magic} \
    --script-valid \
    ${txInArgs} \
    --tx-out ${addr}+2000000 \
    --tx-out-datum-hash ${fuelMarker} \
    --change-address ${addr} \
    --out-file /dev/null | sed 's/Estimated transaction fee: Lovelace //')

echo "Estimated fee: ${estimatedFee}"
outputLovelace=$((${totalLovelace} - ${estimatedFee}))
echo "New total: ${outputLovelace}"

tx=$(mktemp)
cardano-cli transaction build \
    --babbage-era \
    --cardano-mode --epoch-slots 21600 \
    --testnet-magic ${magic} \
    --script-valid \
    ${txInArgs} \
    --tx-out ${addr}+${outputLovelace} \
    --tx-out-datum-hash "a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3" \
    --change-address ${addr} \
    --out-file ${tx}

cardano-cli transaction sign \
    --tx-body-file ${tx} \
    --signing-key-file ${sk} \
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
