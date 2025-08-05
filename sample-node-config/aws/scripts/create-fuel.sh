    #!/usr/bin/env bash
    #
    # Creates a "fuel" UTxO for a Hydra Head from the largest UTxO in the wallet.
    # This script is an adaptation of the one found in the gcp/scripts directory.

    set -e

    source ~/.bash_profile

    # --- Configuration ---
    # The signing key for the wallet.
    SK_FILE=~/credentials/cardano-key.sk

    # The address of the wallet.
    ADDR=$(cat ~/credentials/cardano.addr)

    # The amount of lovelace to place in the fuel UTxO.
    # We'll use 10 ADA here. The rest will be returned as change.
    FUEL_AMOUNT=10000000

    # The required datum hash for a fuel UTxO.
    FUEL_DATUM_HASH="a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3"

    echo "- Using address: ${ADDR}"
    echo "- Using signing key: ${SK_FILE}"

    # --- Find largest UTxO to use as input ---
    echo ""
    echo "- Querying wallet UTxOs..."
    UTXO_JSON=$(ccli query utxo $(ctag) --address ${ADDR} --out-file /dev/stdout)

    if [ -z "$UTXO_JSON" ] || [ "$UTXO_JSON" == "{}" ]; then
      echo "!! ERROR: No UTxOs found at address ${ADDR}."
      echo "!! Please use the faucet to fund this address first."
      exit 1
    fi

    # Find the UTxO with the most lovelace to use as input
    TX_IN=$(echo "${UTXO_JSON}" | jq -r 'to_entries | max_by(.value.value.lovelace) | .key')
    TOTAL_LOVELACE=$(echo "${UTXO_JSON}" | jq -r ".\"${TX_IN}\".value.lovelace")

    echo "- Found largest UTxO:"
    echo "  - TxIn: ${TX_IN}"
    echo "  - Lovelace: ${TOTAL_LOVELACE}"

    if [ "${TOTAL_LOVELACE}" -lt "${FUEL_AMOUNT}" ]; then
        echo "!! ERROR: Insufficient funds. Largest UTxO has ${TOTAL_LOVELACE} lovelace, but need at least ${FUEL_AMOUNT}."
        exit 1
    fi

    # --- Build, Sign, and Submit Transaction ---
    TMP_TX_FILE=$(mktemp)

    echo ""
    echo "- Building transaction..."
    ccli transaction build \
      --babbage-era \
      --tx-in "${TX_IN}" \
      --tx-out "${ADDR}+${FUEL_AMOUNT}" \
      --tx-out-datum-hash "${FUEL_DATUM_HASH}" \
      --change-address "${ADDR}" \
      --out-file "${TMP_TX_FILE}.raw"

    echo "- Signing transaction..."
    ccli transaction sign \
      --tx-body-file "${TMP_TX_FILE}.raw" \
      --signing-key-file "${SK_FILE}" \
      --out-file "${TMP_TX_FILE}.signed"

    echo "- Submitting transaction..."
    ccli transaction submit --tx-file "${TMP_TX_FILE}.signed"

    echo ""
    echo "✔ Transaction submitted successfully!"
    echo "Wait a minute and check your new balance with the 'balance' command."
