#!/usr/bin/env bash

# Prepare a "devnet" directory holding credentials, a dummy topology and
# "up-to-date" genesis files. If the directory exists, it is wiped out.
set -eo pipefail

BASEDIR=${BASEDIR:-$(realpath $(dirname $(realpath $0))/..)}
TARGETDIR=${TARGETDIR:-devnet}

[ -d "$TARGETDIR" ] && { echo "Cleaning up directory $TARGETDIR" ; rm -rf $TARGETDIR ; }

cp -af "$BASEDIR/hydra-cluster/config/devnet" "$TARGETDIR"
chmod -R u+w  "$TARGETDIR"
cp -af "$BASEDIR/hydra-cluster/config/credentials" "$TARGETDIR"
chmod -R u+w "$TARGETDIR"

echo '{"localRoots": [], "publicRoots": []}' > "$TARGETDIR/topology.json"
sed -i.bak "s/\"startTime\": [0-9]*/\"startTime\": $(date +%s)/" "$TARGETDIR/genesis-byron.json" && \
sed -i.bak "s/\"systemStart\": \".*\"/\"systemStart\": \"$(date -u +%FT%TZ)\"/" "$TARGETDIR/genesis-shelley.json"

find $TARGETDIR -type f -name '*.skey' -exec chmod 0400 {} \;

mkdir "$TARGETDIR/ipc"
echo "Prepared devnet, you can start the cluster now"
