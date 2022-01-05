#!/usr/bin/env bash
# Generate coverage report for a single package
# NOTE: This script shell is flacky as support for multi-packages cabal in hpc is somewhat lacking.
# it can fail for a number of reasons, esp, if there are multiple .tix files and conflicting modules
# from different packages. Use with care.

[ $# -eq 1 ] || { echo "Expecting package name, eg. ./coverage.sh hydra-node" ; exit 1 ; }

PACKAGE=$1

MIXDIRS=$(find . -name \*.mix -exec dirname {} \; | uniq | grep $PACKAGE | grep -v 'dyn/')
# It seems like .tix file also references some modules prefixing them with their parent dir
# so to resolve them, we need to point to the parent's parent, not the parent's dir of the
# module
MOREMIXDIRS=$(echo $MIXDIRS | xargs dirname)

[ -n "${MOREMIXDIRS}" ] || { echo "Could not find .mix directories, run 'cabal build $PACKAGE' with coverage on" ; exit 1 ; }

TIXFILES=$(find . -name \*.tix | grep $PACKAGE)

[ -n "${TIXFILES}" ] || { echo "Could not find .tix file, run 'cabal test $PACKAGE' with coverage on" ; exit 1 ; }

# we want to exclude Paths_hydra_node because it has wrong hash value and Main because
# it's uninteresting and can mess up with other Main files
CMDLINE="--destdir html --exclude=Paths_hydra_node --exclude=Main --srcdir=$(pwd)/$PACKAGE"

for m in $MOREMIXDIRS; do
  CMDLINE+=" --hpcdir=$(pwd)/$m"
done

for m in $MIXDIRS; do
  CMDLINE+=" --hpcdir=$(pwd)/$m"
done

exec hpc markup ${CMDLINE} ${TIXFILES}
