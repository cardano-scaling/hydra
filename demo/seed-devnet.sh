#!/usr/bin/env bash

# Seed a "devnet" by distributing some Ada to commit and also some marked as
# "fuel" for the Hydra Head.
set -eo pipefail

MARKER_DATUM_HASH="a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3"
SCRIPT_DIR=$(realpath $(dirname $(realpath $0)))
NETWORK_ID=42

CCLI_PATH=
HYDRA_NODE_PATH=
DEVNET_DIR=/data
if ([[ -n ${1} ]] && $(${1} version > /dev/null)) || ([[ -n ${2} ]] && $(${2} version > /dev/null)); then

  if ([[ -n ${1} ]] && $(${1} version > /dev/null)); then
    CCLI_PATH=${1}
    echo >&2 "Using provided cardano-cli"
  fi

  if [[ -n ${2} ]] && $(${2} --version > /dev/null); then
    HYDRA_NODE_PATH=${2}
    echo >&2 "Using provided hydra-node"
  fi
  if [[ -z "$HYDRA_NODE_PATH" || -z "$CCLI_PATH" ]]; then
    echo >&2 "Please provide both cardano-cli and hydra-node executables if you don't want to use docker."
    exit 1
  fi
  DEVNET_DIR=${SCRIPT_DIR}/devnet
fi


# Invoke cardano-cli in running cardano-node container or via provided cardano-cli
function ccli() {
  ccli_ ${@} --testnet-magic ${NETWORK_ID}
}
function ccli_() {
  if [[ -x ${CCLI_PATH} ]]; then
      cardano-cli ${@}
  else
      docker-compose exec cardano-node cardano-cli ${@}
  fi
}

# Retrieve some lovelace from faucet, marked as "fuel" if requested
function seedFaucet() {
    ACTOR=${1}
    AMOUNT=${2}
    MARKED=${3:-"normal"}
    echo >&2 "Seeding a UTXO from faucet to ${ACTOR} with ${AMOUNT}Ł (${MARKED})"

    # Determine faucet address and just the **first** txin addressed to it
    FAUCET_ADDR=$(ccli address build --payment-verification-key-file ${DEVNET_DIR}/credentials/faucet.vk)
    FAUCET_TXIN=$(ccli query utxo --address ${FAUCET_ADDR} --out-file /dev/stdout | jq -r 'keys[0]')

    ACTOR_ADDR=$(ccli address build --payment-verification-key-file ${DEVNET_DIR}/credentials/${ACTOR}.vk)

    # Optionally mark output
    MARKER=""
    if [[ "${MARKED}" == "fuel" ]]; then
        MARKER="--tx-out-datum-hash ${MARKER_DATUM_HASH}"
    fi

    ccli transaction build --babbage-era --cardano-mode \
        --change-address ${FAUCET_ADDR} \
        --tx-in ${FAUCET_TXIN} \
        --tx-out ${ACTOR_ADDR}+${AMOUNT} \
        ${MARKER} \
        --out-file ${DEVNET_DIR}/seed-${ACTOR}.draft >&2
    ccli transaction sign \
        --tx-body-file ${DEVNET_DIR}/seed-${ACTOR}.draft \
        --signing-key-file ${DEVNET_DIR}/credentials/faucet.sk \
        --out-file ${DEVNET_DIR}/seed-${ACTOR}.signed >&2
    SEED_TXID=$(ccli_ transaction txid --tx-file ${DEVNET_DIR}/seed-${ACTOR}.signed | tr -d '\r')
    SEED_TXIN="${SEED_TXID}#0"
    ccli transaction submit --tx-file ${DEVNET_DIR}/seed-${ACTOR}.signed >&2

    echo -n >&2 "Waiting for utxo ${SEED_TXIN}.."

    while [[ "$(ccli query utxo --tx-in "${SEED_TXIN}" --out-file /dev/stdout | jq ".\"${SEED_TXIN}\"")" = "null" ]]; do
        sleep 1
        echo -n >&2 "."
    done
    echo >&2 "Done"
}

function publishReferenceScripts() {
  echo >&2 "Publishing reference scripts ('νInitial' & 'νCommit')..."
  if [[ -x ${HYDRA_NODE_PATH} ]]; then
    echo $($HYDRA_NODE_PATH publish-scripts \
      --network-id ${NETWORK_ID} \
      --node-socket ${DEVNET_DIR}/ipc/node.socket \
      --cardano-signing-key ${DEVNET_DIR}/credentials/faucet.sk)
  else
    echo $(docker run --rm \
      -v ${PWD}/devnet/credentials:/credentials:ro \
      -v ${PWD}/devnet/ipc:/ipc \
      ghcr.io/input-output-hk/hydra-node -- \
      publish-scripts \
      --network-id ${NETWORK_ID} \
      --node-socket /ipc/node.socket \
      --cardano-signing-key /credentials/faucet.sk)
  fi
}

seedFaucet "alice" 1000000000 # 1000 Ada to commit
seedFaucet "alice" 100000000 "fuel" # 100 Ada marked as "fuel"
seedFaucet "bob" 500000000 # 500 Ada to commit
seedFaucet "bob" 100000000 "fuel" # 100 Ada marked as "fuel"
seedFaucet "carol" 250000000 # 250 Ada to commit
seedFaucet "carol" 100000000 "fuel" # 100 Ada marked as "fuel"
echo "HYDRA_SCRIPTS_TX_ID=$(publishReferenceScripts)" > .env
echo >&2 "Environment variable stored in '.env'"
echo >&2 -e "\n\t$(cat .env)\n"
