#!/usr/bin/env bash
# Agda↔Typst consistency lint.
#
# Full type-checking of the Typst against Agda needs an Agda→Typst backend we
# don't have, so this guards the two single-source links that matter:
#
#  (1) every transition rule the prose cites as `<name>` rule is a real
#      constructor of the Agda relation `_⟶⟨_⟩_`; and
#  (2) the head state-machine DIAGRAM (data in diagrams.typ) has exactly the same
#      (source, rule, target) transitions as that Agda relation — so the picture
#      cannot drift from the formal state machine.
set -euo pipefail
cd "$(dirname "$0")"

ON=src/Hydra/Protocol/OnChain.lagda.typ
DIAG=src/diagrams.typ
fail=0

# --- Agda transition relation: emit "source|rule|target" per constructor ----
agda_transitions() {
  awk '
    /data _⟶⟨_⟩_ :/ { inrel=1; next }
    inrel && /^```/  { inrel=0 }
    !inrel           { next }
    /^  [a-z]/       { if (name != "") emit(); name=$1; buf=$0; next }
                     { buf = buf " " $0 }
    END              { if (name != "") emit() }
    function emit(   a, b, s, t) {
      s = (match(buf, /→[ ]*([A-Za-z]+)/, a)   ? a[1] : "?")
      t = (match(buf, /⟩[ ]*([A-Za-z]+)/, b)   ? b[1] : "?")
      print s "|" name "|" t
    }
  ' "$ON" | sort -u
}

# --- Diagram data: emit "from|rule|to" per transition --------------------------
diagram_transitions() {
  grep -oE '\(from: "[A-Za-z]+", rule: "[A-Za-z]+", to: "[A-Za-z]+"' "$DIAG" \
    | sed -E 's/.*from: "([A-Za-z]+)", rule: "([A-Za-z]+)", to: "([A-Za-z]+)".*/\1|\2|\3/' \
    | sort -u
}

# --- Check (1): cited rule names exist ----------------------------------------
rules=$(agda_transitions | cut -d'|' -f2 | sort -u)
cited=$(grep -oE '`[A-Za-z]+` rule' "$ON" | sed -E 's/`([A-Za-z]+)` rule/\1/' | sort -u)
for c in $cited; do
  if ! grep -qx "$c" <<<"$rules"; then
    echo "ERROR: prose cites transition rule '$c' that is not a constructor of _⟶⟨_⟩_"
    fail=1
  fi
done

# --- Check (2): diagram transitions == Agda transitions ------------------------
only_agda=$(comm -23 <(agda_transitions) <(diagram_transitions))
only_diag=$(comm -13 <(agda_transitions) <(diagram_transitions))
if [ -n "$only_agda" ]; then
  echo "ERROR: transitions in the Agda relation but MISSING from the diagram (diagrams.typ):"
  echo "$only_agda" | sed 's/^/  /'
  fail=1
fi
if [ -n "$only_diag" ]; then
  echo "ERROR: transitions in the diagram but NOT in the Agda relation _⟶⟨_⟩_:"
  echo "$only_diag" | sed 's/^/  /'
  fail=1
fi

# --- Check (3): diagrams.typ `state-fields` keys == HeadDatum constructors -----
datum_ctors=$(awk '
  /data HeadDatum :/                  { ind=1; next }
  ind && (/^```/ || /^$/ || /^data /) { ind=0 }
  ind && /^  [A-Z]/                   { print $1 }
' "$ON" | sort -u)
state_keys=$(awk '
  /#let state-fields = \(/ { ins=1; next }
  ins && /^\)/             { ins=0 }
  ins && /^  "[A-Za-z]+":/ { gsub(/[",:]/,"",$1); print $1 }
' "$DIAG" | sort -u)
if [ -n "$datum_ctors" ] && [ "$datum_ctors" != "$state_keys" ]; then
  echo "ERROR: diagrams.typ state-fields keys do not match HeadDatum constructors:"
  echo "  HeadDatum: $(echo $datum_ctors)"
  echo "  state-fields: $(echo $state_keys)"
  fail=1
fi

# --- Check (4): every tx-rule diagram maps to a real transition rule -----------
tx_rules=$(awk '
  /#let tx-rule = \(/ { intr=1; next }
  intr && /^\)/       { intr=0 }
  intr && /: "[A-Za-z]+"/ { match($0, /"([A-Za-z]+)"/, a); print a[1] }
' "$DIAG" | sort -u)
for r in $tx_rules; do
  if ! grep -qx "$r" <<<"$rules"; then
    echo "ERROR: diagrams.typ tx-rule maps to '$r', not a constructor of _⟶⟨_⟩_"
    fail=1
  fi
done

if [ "$fail" -eq 0 ]; then
  echo "check-refs: OK — cited rules exist, the head-state diagram matches the Agda"
  echo "relation, state-fields match HeadDatum, and tx diagrams map to real rules."
fi
exit "$fail"
