#!/usr/bin/env bash
#
# Manually invite Carol to join the open head between Alice and Bob.
#
# Prerequisites:
#   * 'nix run .#demo' is running and the head between Alice and Bob is Open
#     (drive the head via the alice / bob TUIs: 'i' to Init, then commit
#     funds and wait for HeadIsOpen).
#   * 'jq' and 'curl' on PATH.
#   * 'cardano-cli' on PATH (provided by the demo's nix shell).
#
# This issues a POST to Alice's API. Both Alice and Bob will multi-sign a
# snapshot whose 'parameterUpdate' is 'AddParty carol' and post an
# UpdateParametersTx to the L1 chain; once observed, both nodes will emit
# 'JoinFinalized' and the head's parties list will be [alice, bob, carol].

set -euo pipefail

ALICE_API="${ALICE_API:-http://127.0.0.1:4001}"

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

CAROL_HYDRA_VK="$REPO_ROOT/demo/carol.vk"
CAROL_CARDANO_VK="$REPO_ROOT/hydra-cluster/config/credentials/carol.vk"

if [[ ! -f $CAROL_HYDRA_VK || ! -f $CAROL_CARDANO_VK ]]; then
  echo "Error: cannot find carol's keys; expected:" >&2
  echo "  $CAROL_HYDRA_VK" >&2
  echo "  $CAROL_CARDANO_VK" >&2
  exit 1
fi

# Carol's Hydra verification key. Hydra key files store the raw key as a
# CBOR byte string with a 4-character prefix (0x58 0x20 = bstr, length 32);
# strip it to get the 64-hex-char key.
HYDRA_VK_HEX=$(jq -r '.cborHex' <"$CAROL_HYDRA_VK")
JOINING_PARTY_VKEY="${HYDRA_VK_HEX#5820}"

# Carol's OnChainId is the cardano payment-key hash of her fuel key (28
# bytes = 56 hex chars).
JOINING_ON_CHAIN_ID=$(
  cardano-cli address key-hash \
    --payment-verification-key-file "$CAROL_CARDANO_VK"
)

# Carol's L2 network host (etcd peer URL). Must match the @--listen@ /
# @--advertise@ value her hydra-node will use when started — see
# 'nix/hydra/demo.nix'. Alice and bob's hydra-nodes will run
# 'etcdctl member add' for this host so carol can join the L2 mesh.
JOINING_HOST="${JOINING_HOST:-127.0.0.1:5003}"

echo "Inviting carol with:"
echo "  joiningParty.vkey  = $JOINING_PARTY_VKEY"
echo "  joiningOnChainId   = $JOINING_ON_CHAIN_ID"
echo "  joiningHost        = $JOINING_HOST"
echo "Posting to $ALICE_API/participants ..."

curl -sS -X POST \
  -H 'Content-Type: application/json' \
  --data "$(jq -n \
    --arg vkey "$JOINING_PARTY_VKEY" \
    --arg oid "$JOINING_ON_CHAIN_ID" \
    --arg host "$JOINING_HOST" \
    '{joiningParty: {vkey: $vkey}, joiningOnChainId: $oid, joiningHost: $host}')" \
  "$ALICE_API/participants"

echo
echo "Watch ./devnet/alice-logs.txt and ./devnet/bob-logs.txt for"
echo "'JoinRequested' / 'JoinApproved' / 'JoinFinalized' events."
echo
echo "Or stream the API output:"
echo "  websocat ws://127.0.0.1:4001  # alice"
echo "  websocat ws://127.0.0.1:4002  # bob"
