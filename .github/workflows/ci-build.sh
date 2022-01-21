#!/usr/bin/env bash
set -e

cabal update
cabal build --enable-tests all
