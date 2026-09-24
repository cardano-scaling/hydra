#!/usr/bin/env bash
# Interactive menu for one external (non-party) user driving a mediator node.
#
# Usage: delegated-demo/actor.sh <name> <api-port>
#   e.g. delegated-demo/actor.sh anna 4001
#        delegated-demo/actor.sh elsa 4002
#
# Run from the repository root, after the cluster is up.

HERE=$(dirname "$(realpath "$0")")
# shellcheck source=/dev/null
source "$HERE/lib.sh"

NAME=${1:?usage: actor.sh <name> <api-port>}
PORT=${2:?usage: actor.sh <name> <api-port>}

# The counterpart user (for the send action).
if [ "$NAME" = "anna" ]; then OTHER=elsa; else OTHER=anna; fi

status() {
  echo
  echo "=================== $NAME (mediator :$PORT) ==================="
  if head_is_open "$PORT"; then echo "head: OPEN"; else echo "head: not open (use [i])"; fi
  echo "L1   balance: $(ada "$(l1_balance "$NAME" 2>/dev/null || echo 0)")"
  echo "head balance: $(ada "$(head_balance "$PORT" "$NAME" 2>/dev/null || echo 0)")"
  echo "address: $(addr_of "$NAME" 2>/dev/null)"
  echo "--------------------------------------------------------------"
  echo " [i] init head     [c] commit funds into head (via mediator)"
  [ "$NAME" = elsa ] && echo " [d] deposit on L1 directly (client-side, no operator)"
  echo " [s] send ada to $OTHER    [w] withdraw all to L1"
  echo " [l] refresh       [q] quit"
}

while true; do
  status
  read -rp "$NAME> " choice rest
  case "$choice" in
    i) init_head "$PORT" ;;
    c) commit "$PORT" "$NAME" ;;
    d)
      if [ "$NAME" = elsa ]; then "$HERE/elsa-deposit.sh" "$PORT"
      else echo "client-side deposit is only wired up for elsa"; fi
      ;;
    s)
      amount="$rest"
      [ -z "$amount" ] && read -rp "lovelace to send to $OTHER: " amount
      send "$PORT" "$NAME" "$OTHER" "$amount"
      ;;
    w) withdraw "$PORT" "$NAME" ;;
    l) ;;
    q) exit 0 ;;
    *) echo "unknown option: $choice" ;;
  esac
done
