#!/usr/bin/env bash

set -eo pipefail

SCRIPT_DIR=${SCRIPT_DIR:-$(realpath $(dirname $(realpath $0)))}
NETWORK_ID=42

CCLI_CMD=
DEVNET_DIR=/devnet
if [[ -n ${1} ]]; then
    echo >&2 "Using provided cardano-cli command: ${1}"
    $(${1} version > /dev/null)
    CCLI_CMD=${1}
    DEVNET_DIR=${SCRIPT_DIR}/devnet
fi

HYDRA_NODE_CMD=
if [[ -n ${2} ]]; then
    echo >&2 "Using provided hydra-node command: ${2}"
    ${2} --version > /dev/null
    HYDRA_NODE_CMD=${2}
fi

# Invoke hydra-node in a container or via provided executable
function hnode() {
  if [[ -n ${HYDRA_NODE_CMD} ]]; then
      ${HYDRA_NODE_CMD} ${@}
  else
      docker run --rm \
        --pull always \
        -v ${SCRIPT_DIR}/devnet:/devnet \
        ghcr.io/cardano-scaling/hydra-node:0.18.1 -- ${@}
  fi
}

function publishReferenceScripts() {
  echo >&2 "Publishing reference scripts..."
  hnode publish-scripts \
    --testnet-magic ${NETWORK_ID} \
    --node-socket ${DEVNET_DIR}/node.socket \
    --cardano-signing-key devnet/credentials/faucet.sk
}

# Invoke cardano-cli in running cardano-node container or via provided cardano-cli
function ccli() {
  ccli_ ${@} --testnet-magic ${NETWORK_ID}
}
function ccli_() {
  if [[ -x ${CCLI_CMD} ]]; then
      ${CCLI_CMD} ${@}
  else
      ${DOCKER_COMPOSE_CMD} exec cardano-node cardano-cli ${@}
  fi
}

function queryPParams() {
  echo >&2 "Query Protocol parameters"
  if [[ -x ${CCLI_CMD} ]]; then
     ccli query protocol-parameters --socket-path ${DEVNET_DIR}/node.socket  --out-file /dev/stdout \
      | jq ".txFeeFixed = 0 | .txFeePerByte = 0 | .executionUnitPrices.priceMemory = 0 | .executionUnitPrices.priceSteps = 0" > devnet/protocol-parameters.json
   else
     docker exec demo-cardano-node-1 cardano-cli query protocol-parameters --testnet-magic ${NETWORK_ID} --socket-path ${DEVNET_DIR}/node.socket --out-file /dev/stdout \
      | jq ".txFeeFixed = 0 | .txFeePerByte = 0 | .executionUnitPrices.priceMemory = 0 | .executionUnitPrices.priceSteps = 0" > devnet/protocol-parameters.json
  fi
  echo >&2 "Saved in protocol-parameters.json"
}

queryPParams

echo "HYDRA_SCRIPTS_TX_ID=$(publishReferenceScripts)" > .env
echo >&2 "Environment variable stored in '.env'"
echo >&2 -e "\n\t$(cat .env)\n"
