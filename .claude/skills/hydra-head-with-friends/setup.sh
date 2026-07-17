#!/usr/bin/env bash
#
# One-time setup for your "Hydra head with friends" node on preprod.
# Run once before `docker compose up`. Safe to re-run: it skips work already done.
#
#   1. fetches the Cardano preprod node configuration
#   2. generates your Cardano + Hydra keys (via the pinned Docker images)
#   3. prints the layer-1 addresses you need to fund from the faucet
#   4. fast-bootstraps the Cardano database from a Mithril snapshot
#
set -euo pipefail
cd "$(dirname "$0")"

if [ ! -f .env ]; then
  echo "No .env found. Run:  cp .env.example .env   then edit it."
  exit 1
fi
set -a; . ./.env; set +a
: "${MY_NAME:?set MY_NAME in .env}"
: "${CARDANO_NODE_VERSION:?set CARDANO_NODE_VERSION in .env}"
: "${HYDRA_VERSION:?set HYDRA_VERSION in .env}"
: "${MITHRIL_IMAGE:?set MITHRIL_IMAGE in .env}"

mkdir -p cardano-conf data/db data/ipc credentials peers persistence

CNODE="ghcr.io/intersectmbo/cardano-node:${CARDANO_NODE_VERSION}"
HNODE="ghcr.io/cardano-scaling/hydra-node:${HYDRA_VERSION}"
UIDGID="$(id -u):$(id -g)"

# --- 1. Cardano preprod node configuration ----------------------------------
echo "==> Fetching Cardano preprod configuration ..."
base="https://book.play.dev.cardano.org/environments/preprod"
for f in config topology byron-genesis shelley-genesis alonzo-genesis conway-genesis; do
  [ -f "cardano-conf/${f}.json" ] || curl -fsSL -o "cardano-conf/${f}.json" "${base}/${f}.json"
done

# --- 2. Keys ----------------------------------------------------------------
# cardano-cli (fuel + funds) via the cardano-node image; hydra keys via hydra-node.
cardano_keypair() { # $1 = basename, e.g. alice-node
  [ -f "credentials/$1.sk" ] && return 0
  echo "==> Generating Cardano key: $1"
  docker run --rm --user "$UIDGID" --entrypoint cardano-cli \
    -v "$PWD/credentials:/out" -w /out "$CNODE" \
    address key-gen --verification-key-file "$1.vk" --signing-key-file "$1.sk"
}
cardano_keypair "${MY_NAME}-node"    # layer-1 identity + fee "fuel"
cardano_keypair "${MY_NAME}-funds"   # what you commit into and spend in the head

if [ ! -f "credentials/${MY_NAME}-hydra.sk" ]; then
  echo "==> Generating Hydra key: ${MY_NAME}-hydra"
  docker run --rm --user "$UIDGID" \
    -v "$PWD/credentials:/out" "$HNODE" \
    gen-hydra-key --output-file "/out/${MY_NAME}-hydra"
fi

# Publish your two PUBLIC keys so the shared peers block resolves, and so you
# can hand these exact files to your friends. (.vk files are safe to share.)
cp -f "credentials/${MY_NAME}-node.vk"  "peers/${MY_NAME}-node.vk"
cp -f "credentials/${MY_NAME}-hydra.vk" "peers/${MY_NAME}-hydra.vk"

# --- 3. Addresses to fund ---------------------------------------------------
address() { # $1 = basename
  docker run --rm --entrypoint cardano-cli -v "$PWD/credentials:/c" "$CNODE" \
    address build --testnet-magic 1 --payment-verification-key-file "/c/$1.vk"
}
echo
echo "==> Fund these preprod addresses from the faucet:"
echo "    https://docs.cardano.org/cardano-testnets/tools/faucet/ (select Pre-Production)"
echo
echo "    node  (fuel, >= 30 tADA): $(address "${MY_NAME}-node")"
echo "    funds (to play with)    : $(address "${MY_NAME}-funds")"
echo

# --- 4. Mithril fast-bootstrap of the Cardano DB ----------------------------
if [ -n "$(ls -A data/db 2>/dev/null || true)" ]; then
  echo "==> data/db already populated; skipping Mithril download."
else
  echo "==> Downloading a Mithril snapshot of the preprod chain (several GB) ..."
  gvk="$(curl -fsSL https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey)"
  avk="$(curl -fsSL https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/ancillary.vkey)"
  docker run --rm \
    -e AGGREGATOR_ENDPOINT="https://aggregator.release-preprod.api.mithril.network/aggregator" \
    -e GENESIS_VERIFICATION_KEY="$gvk" \
    -e ANCILLARY_VERIFICATION_KEY="$avk" \
    -v "$PWD/data:/data" -w /data "$MITHRIL_IMAGE" \
    cardano-db download --include-ancillary --origin-tag HYDRA latest

  # Normalize whatever layout Mithril produced into data/db.
  if [ ! -e data/db/immutable ]; then
    found="$(find data -maxdepth 5 -type d -name immutable 2>/dev/null | head -n1 || true)"
    if [ -n "$found" ]; then
      src="$(dirname "$found")"
      echo "==> Moving downloaded DB from ${src} to data/db"
      rm -rf data/db && mv "$src" data/db
    else
      echo "!! Could not locate the downloaded DB (no 'immutable' dir under data/)."
      echo "!! Inspect data/ and move the chain DB to data/db, or sync from genesis."
    fi
  fi
fi

echo
echo "Done. Next:"
echo "  1. Send peers/${MY_NAME}-node.vk and peers/${MY_NAME}-hydra.vk to your friends,"
echo "     and drop the .vk files they send you into peers/ ."
echo "  2. Agree on everyone's PUBLIC host:port and fill in config/hydra-node.yaml"
echo "     (advertise + the peers block) identically across the group."
echo "  3. docker compose up -d cardano-node   # wait until fully synced"
echo "  4. docker compose up -d hydra-node"
