#!/usr/bin/env bash
#
# Fork off some amount of the UTxO owned by given signing key and mark the rest
# as fuel to be used by the hydra-node.
set -evx

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

export  CARDANO_NODE_SOCKET_PATH="${testnetDir}/node.socket"
echo "Wait for socket to appear ${CARDANO_NODE_SOCKET_PATH}"

while ! [[ -S "${CARDANO_NODE_SOCKET_PATH}" ]]; do
  echo -n '.'
  sleep 1
done

magic=$(cat ${testnetDir}/db/protocolMagicId)

# Invoke cardano-cli in running cardano-node container or via provided cardano-cli
function ccli() {
   ccli_ ${@}
}

function ccli_() {
  if [[ -x ${CCLI_CMD} ]]; then
      ${CCLI_CMD} ${@}
  else
      docker-compose exec cardano-node cardano-cli ${@}
  fi
}

vk=${sk%.sk}.vk
if [[ ! -f "${vk}" ]]; then
  ccli key verification-key --signing-key-file ${sk} --verification-key-file ${vk}
fi

addr=$(ccli address build --payment-verification-key-file $vk --testnet-magic ${magic})

utxo=$(ccli query utxo \
    --cardano-mode --epoch-slots 21600 \
    --testnet-magic ${magic} \
    --address ${addr} \
    --out-file /dev/stdout)
totalLovelace=$(echo ${utxo} | jq -r 'reduce .[] as $item (0; . + $item.value.lovelace)')
countdown=30

echo "Waiting for funds to appear at address ${addr}"

while [[ ${totalLovelace} -eq 0 ]] && [[ ${countdown} -gt 0 ]]; do
  sleep 5
  utxo=$(ccli query utxo \
              --cardano-mode --epoch-slots 21600 \
              --testnet-magic ${magic} \
              --address ${addr} \
              --out-file /dev/stdout)
  totalLovelace=$(echo ${utxo} | jq -r 'reduce .[] as $item (0; . + $item.value.lovelace)')
  countdown=$((countdown - 1))
done

[[ ${countdown} -eq 0 ]] && echo "Error: insufficient funds" && exit 1

entries=$(echo ${utxo} | jq "to_entries|sort_by(.value.value.lovelace)|last")
input=$(echo ${entries} | jq '.key' | tr -d '"')
fuelAmount=$(echo ${entries} | jq ".value.value.lovelace - ${amount}")

tx=$(mktemp)
ccli transaction build \
     --testnet-magic ${magic} \
     --babbage-era \
     --cardano-mode --epoch-slots 21600 \
     --script-valid \
     --tx-in ${input} \
     --tx-out ${addr}+${fuelAmount} \
     --tx-out-datum-hash "a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3" \
     --change-address ${addr} \
     --out-file ${tx}

ccli transaction sign \
     --testnet-magic ${magic} \
     --tx-body-file ${tx} \
     --signing-key-file $sk \
     --out-file ${tx}.signed

ccli transaction submit \
    --cardano-mode \
    --epoch-slots 21600 \
    --testnet-magic ${magic} \
    --tx-file ${tx}.signed
