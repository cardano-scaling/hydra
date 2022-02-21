#!/usr/bin/env bash

# Prepare a "devnet" directory holding credentials, a dummy topology and
# "up-to-date" genesis files. If the directory exists, it is wiped out.
set -e

case $0 in
  /*) BASEDIR=$(dirname $0)/..;;
  *) BASEDIR=$(dirname $PWD/$0)/..;;
esac

TARGETDIR="devnet"

[ -d "$TARGETDIR" ] && { echo "Cleaning up directory $TARGETDIR" ; sudo rm -r $TARGETDIR ; }

cp -af "$BASEDIR/hydra-cluster/config" "$TARGETDIR"
find $TARGETDIR -type f -exec chmod 0400 {} \;

mkdir $TARGETDIR/node-1
cat > $TARGETDIR/node-1/network-topology.json <<EOF
[{"hostname":"hydra-node-2","port":5001},{"hostname":"hydra-node-3","port":5001}]
EOF

mkdir $TARGETDIR/node-2
cat > $TARGETDIR/node-2/network-topology.json <<EOF
[{"hostname":"hydra-node-1","port":5001},{"hostname":"hydra-node-3","port":5001}]
EOF

mkdir $TARGETDIR/node-3
cat > $TARGETDIR/node-3/network-topology.json <<EOF
[{"hostname":"hydra-node-1","port":5001},{"hostname":"hydra-node-2","port":5001}]
EOF

echo '{"Producers": []}' > "$TARGETDIR/topology.json"
sed -i "s/\"startTime\": [0-9]*/\"startTime\": $(date +%s)/" "$TARGETDIR/genesis-byron.json" && \
sed -i "s/\"systemStart\": \".*\"/\"systemStart\": \"$(date -u +%FT%TZ)\"/" "$TARGETDIR/genesis-shelley.json"
echo "Prepared devnet, you can start the cluster now"
