#!/usr/bin/env bash
# Live L1 vs head balances for the two external users. Run from the repo root.
HERE=$(dirname "$(realpath "$0")")
# shellcheck source=/dev/null
source "$HERE/lib.sh"

row() { # name port
  printf "%-6s %18s %18s\n" "$1" \
    "$(ada "$(l1_balance "$1" 2>/dev/null || echo 0)")" \
    "$(ada "$(head_balance "$2" "$1" 2>/dev/null || echo 0)")"
}

while true; do
  clear
  head_is_open "$ANNA_PORT" && st=OPEN || st=closed
  echo "delegated-demo observer     head: $st        $(date +%T)"
  echo "mediators: alice :4001  bob :4002  carol :4003 (no funds of their own)"
  echo
  printf "%-6s %18s %18s\n" user L1 head
  row anna "$ANNA_PORT"
  row elsa "$ELSA_PORT"
  sleep 3
done
