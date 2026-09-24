#!/usr/bin/env bash
# Non-interactive happy path for the delegated demo, with on-chain assertions.
#
# Assumes the cluster is already running (nix run .#delegated-demo, or the tmux
# entrypoint). Run from the repository root. Exits non-zero on any failure.
#
#   Init -> anna commits -> elsa commits -> anna sends 100 to elsa
#        -> assert head balances -> elsa withdraws to L1 -> assert L1 balance
#
# Anna and Elsa are seeded with different amounts (see seed-devnet.sh), so the
# committed amounts are read from the head rather than hard-coded.
set -eo pipefail

HERE=$(dirname "$(realpath "$0")")
# shellcheck source=/dev/null
source "$HERE/lib.sh"

XFER=100000000     # 100 ada anna -> elsa on L2

assert_eq() { # label actual expected
  if [ "$2" = "$3" ]; then
    echo "  ok: $1 = $(ada "$2")"
  else
    echo "  FAIL: $1 = $(ada "$2"), expected $(ada "$3")" >&2
    exit 1
  fi
}

echo "== opening the head via mediator alice (:$ANNA_PORT) =="
init_head "$ANNA_PORT"

echo "== L1 balances before =="
echo "  anna: $(ada "$(l1_balance anna)")   elsa: $(ada "$(l1_balance elsa)")"
ELSA_L1_START=$(l1_balance elsa)

echo "== anna commits into the head via mediator alice =="
commit "$ANNA_PORT" anna
echo "== elsa commits into the head via mediator bob =="
commit "$ELSA_PORT" elsa

# Read what each actually committed (their largest L1 UTxO).
ANNA_COMMIT=$(head_balance "$ANNA_PORT" anna)
ELSA_COMMIT=$(head_balance "$ELSA_PORT" elsa)
echo "  anna committed $(ada "$ANNA_COMMIT"); elsa committed $(ada "$ELSA_COMMIT")"

echo "== anna pays elsa $(ada "$XFER") on L2 =="
send "$ANNA_PORT" anna elsa "$XFER"

echo "== asserting in-head balances =="
assert_eq "anna head balance" "$(head_balance "$ANNA_PORT" anna)" "$((ANNA_COMMIT - XFER))"
assert_eq "elsa head balance" "$(head_balance "$ELSA_PORT" elsa)" "$((ELSA_COMMIT + XFER))"

echo "== elsa withdraws her head funds back to L1 via mediator bob =="
withdraw "$ELSA_PORT" elsa

echo "== asserting elsa's L1 balance grew by the withdrawn amount =="
# elsa moved ELSA_COMMIT into the head, then pulled ELSA_COMMIT + XFER back out,
# so her L1 balance nets up by exactly XFER (anna's payment).
ELSA_L1_END=$(l1_balance elsa)
echo "  elsa L1: $(ada "$ELSA_L1_START") -> $(ada "$ELSA_L1_END")"
assert_eq "elsa L1 balance" "$ELSA_L1_END" "$((ELSA_L1_START + XFER))"

echo
echo "SUCCESS: elsa received $(ada "$XFER") from anna over the head and settled it on L1."
echo "The head is still open; run delegated-demo/actor.sh to keep transacting."
