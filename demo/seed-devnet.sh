#!/usr/bin/env bash

# Seed a "devnet" by marking some ADA as "payment outputs" for the Hydra Head
# ("the fuel tank").
set -e

TESTNET_MAGIC=42
MARKER_DATUM_HASH="a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3"
DEVNET_DIR=/data

function ccli() {
  # Invoke cardano-cli in running cardano-node container
  CMD="CARDANO_NODE_SOCKET_PATH=${DEVNET_DIR}/ipc/node.socket cardano-cli ${@} --testnet-magic 42"
  docker-compose exec cardano-node sh -c "${CMD}"
}

function seedPayment() {
    ACTOR=${1}
    AMOUNT=${2}
    echo >&2 "Seeding a payment UTXO for ${ACTOR} with ${AMOUNT} lovelace"

    ADDR=$(ccli address build --payment-verification-key-file ${DEVNET_DIR}/credentials/${ACTOR}.vk)
    GENESIS_TXIN=$(ccli genesis initial-txin --verification-key-file ${DEVNET_DIR}/credentials/${ACTOR}-genesis.vk | tr -d '\n\r')

    ccli transaction build --alonzo-era --cardano-mode \
        --change-address ${ADDR} \
        --tx-in ${GENESIS_TXIN} \
        --tx-out ${ADDR}+${AMOUNT} \
        --tx-out-datum-hash ${MARKER_DATUM_HASH} \
        --out-file ${DEVNET_DIR}/${ACTOR}.draft
    ccli transaction sign \
        --tx-body-file ${DEVNET_DIR}/${ACTOR}.draft \
        --signing-key-file ${DEVNET_DIR}/credentials/${ACTOR}.sk \
        --out-file ${DEVNET_DIR}/${ACTOR}.signed
    ccli transaction submit --tx-file ${DEVNET_DIR}/${ACTOR}.signed
}

seedPayment "alice" 1000000000
seedPayment "bob" 500000000
seedPayment "carol" 250000000
