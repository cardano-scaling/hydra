#!/usr/bin/env bash
# Update nix/ directory  materialising dependencies (whatever that means)
# NOTE: This script is very simple and naive

sed -i -e 's/checkMaterialization = false/checkMaterialization = true/g' default.nix

tmp=$(mktemp)

nix-build -A hydra-node.components.exes.hydra-node 2>&1 | tee $tmp

NEW_SHA=$(grep 'got:    sha256:' $tmp | sed -e 's/.*sha256:\(.*\)/\1/')

[ -n "$NEW_SHA" ]  && sed -i -e "s/\(.*\)plan-sha256 =.*/\1plan-sha256 = \"$NEW_SHA\"\;/g" default.nix

nix-build -A hydra-node.project.plan-nix.passthru.updateMaterialized 2> $tmp | bash

NIX_SCRIPT=$(grep 'To fix run' $tmp |  sed -e 's/.*To fix run: \(.*\)/\1/')

echo "Running $NIX_SCRIPT"

eval "${NIX_SCRIPT}"

sed -i -e 's/checkMaterialization = true/checkMaterialization = false/g' default.nix

rm $tmp
