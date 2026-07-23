#!/usr/bin/env python3
"""Stage 3 of build.sh: hover-tooltips for the spec's notation symbols.

Stamps an invisible highlight annotation (opacity 0, plain-text definition in
/Contents) over every prose occurrence of the notation symbols below. Viewers
that show annotation popups on hover (okular, pdf.js - including the docs-site
iframe - and Acrobat; macOS Preview needs a click) then display the definition
when the reader hovers the symbol. The rendered page is pixel-identical.

Matching notes:
- Typst renders prose math with the MATHEMATICAL ITALIC / SANS codepoints
  (eta is U+1D702, not U+03B7; cid is U+1D5BC.., not ASCII), while code blocks
  use plain letters in JuliaMono - and JuliaMono spans are skipped explicitly -
  so the tooltip layer touches prose and figure math only.
- Matches are found by walking per-character text (get_text "rawdict") line by
  line, NOT by substring search: a single-letter token only matches when its
  neighbours are not same-size letters of the same alphabet. This keeps the
  "s" inside the math-italic word "closeTx" or the "st" inside sans "contest"
  from being annotated, while still matching decorated true occurrences like
  v', (eta')#, U_alpha or k_H (primes, hashes and subscripts are either
  non-letters or sub-size).

Usage: annotate-notation.py IN.pdf OUT.pdf
Set ANNOTATE_NOTATION=skip in build.sh to skip the stage (the nix build runs
this as a separate derivation; see nix/hydra/spec.nix).
Needs PyMuPDF.
"""

import sys

import fitz  # PyMuPDF

# token -> tooltip text (plain unicode; PDF /Contents cannot render math).
# Tokens use the codepoints typst actually emits (math italic / math sans).
DEFS = {
    # -- protocol objects ---------------------------------------------------
    "\U0001D702": (  # 𝜂 eta
        "η — the accumulator commitment to the head's UTxO set (KZG, §3.4). "
        "η# = hash(η) is the hash bound into the snapshot signature; η′ is "
        "the produced state's commitment; ηΔ the pending-delta commitment."
    ),
    "\U0001D709": (  # 𝜉 xi
        "ξ — the snapshot multisignature: an aggregate signature by all n "
        "parties over cid ‖ v ‖ s ‖ η# ‖ δ# ‖ κ# (§3.2, §5.4–§5.7)."
    ),
    "\U0001D6FF": (  # 𝛿 delta
        "δ# — hash of the snapshot's decommit output set "
        "(decommitOutputsHash). Bound into the signed message; decrement "
        "recomputes it on-chain from the tx's decommit outputs."
    ),
    "\U0001D705": (  # 𝜅 kappa
        "κ# — hash of the snapshot's commit output set (commitOutputsHash). "
        "Bound into the signed message; increment recomputes it on-chain "
        "from the claimed deposit's datum."
    ),
    "₳": (
        "𝒜ₒ — the ADA overhead: head-UTxO lovelace not belonging to any L2 "
        "UTxO (min-UTxO deposit), fixed at init, preserved by every "
        "transition, released to the poster at fanout (§5.6)."
    ),
    "\U0001D711": (  # 𝜑 phi
        "φ — a transaction output reference (tx id + output index): "
        "φ_deposit the deposit being claimed, φ_increment the redeemer's "
        "claimed reference, φ_seed the init seed."
    ),
    "\U0001D70B": (  # 𝜋 pi
        "π — the KZG membership witness: proves the distributed outputs are "
        "members of the accumulator η (§3.4, §5.8)."
    ),
    "\U0001D707": (  # 𝜇 mu
        "μ_head — the head's minting policy, parameterised by the seed "
        "output reference; cid = hash(μ_head(seed)) (§5.1)."
    ),
    "\U0001D708": (  # 𝜈 nu
        "ν — the validator scripts: ν_head the head state machine, "
        "ν_deposit the deposit validator (§5)."
    ),
    "𝖼𝗂𝖽": (
        "cid — the head's currency id, hash(μ_head(seed)): unique per head "
        "instance, names the ST/PT tokens, first component of the signed "
        "snapshot message (§5.1)."
    ),
    "𝖲𝖳": (
        "ST — the state-thread token {cid ↦ 'HydraHeadV2' ↦ 1}: marks the "
        "head output and ensures contract continuity (§5.1)."
    ),
    "𝖯𝖳": (
        "PT — a participation token {cid ↦ k# ↦ 1}, one per party (token "
        "name = the party's key hash): authenticates head transactions "
        "(§5.1)."
    ),
    # -- scalars ------------------------------------------------------------
    "\U0001D463": (  # 𝑣 v
        "v — the open-state version: bumped by increment/decrement, "
        "preserved by close/contest; snapshots sign against it."
    ),
    "\U0001D460": (  # 𝑠 s
        "s — the snapshot number: strictly increasing per snapshot; "
        "contest requires a higher s than the stored one."
    ),
    "\U0001D45B": (  # 𝑛 n
        "n — the number of head parties; init mints n+1 tokens (ST + one "
        "PT per party), fanout burns them."
    ),
    "\U0001D45A": (  # 𝑚 m
        "m — the number of outputs distributed (fanout/partial fanout) or "
        "decommitted (decrement) by the transaction."
    ),
    "\U0001D458": (  # 𝑘 k
        "k — keys: k_H the aggregate Hydra key of the multisignature "
        "scheme; k_i a participant's key, k# its hash (the PT token name)."
    ),
    "\U0001D461": (  # 𝑡 t
        "t — time points: t_min/t_max the tx validity bounds, t_final the "
        "contestation deadline, t_recover the deposit recover deadline, "
        "t_created the deposit creation time."
    ),
    "\U0001D436": (  # 𝐶 C
        "C — a deposit's commit list: the deposited output references with "
        "their serialised outputs, recorded in the deposit datum (§5.2)."
    ),
    "\U0001D49E": (  # 𝒞 script C
        "𝒞 — the contester set of a closed head: key hashes of parties "
        "that contested; grows by exactly one per contest (§5.7)."
    ),
    "Δ": (
        "Δ — the pending increment/decrement delta: ηΔ is the accumulator "
        "commitment of the pending UTxOs a Used close/contest combines in "
        "(§5.6)."
    ),
    # -- operators ----------------------------------------------------------
    "∘": (
        "∘ — ledger transaction application: U ∘ tx applies tx to the UTxO "
        "set U, yielding the updated set or ⊥ on conflict (§6)."
    ),
    "‖": (
        "‖ — byte-serialisation concatenation (§3.1): the signed snapshot "
        "message is cid ‖ v ‖ s ‖ η# ‖ δ# ‖ κ#."
    ),
    "⊥": (
        "⊥ — no value / failure: a conflicting ledger application, or an "
        "empty optional slot (no pending deposit/decommit)."
    ),
}

# Math-alphanumeric ranges whose same-size adjacency BLOCKS a match (a token
# inside a longer math word, e.g. the s in closeTx or the st in contest).
_LATIN_ITALIC = tuple(range(0x1D434, 0x1D468))  # A-Z, a-z math italic
_SANS = tuple(range(0x1D5A0, 0x1D608))  # math sans A-Z a-z


def _is_blocking(ch: str, size: float, tok_size: float, sans: bool) -> bool:
    if ch is None or not ch.strip():
        return False
    cp = ord(ch)
    letters = _SANS if sans else _LATIN_ITALIC
    return cp in letters and size >= 0.85 * tok_size


def _line_chars(page):
    """Yield per-line lists of (char, rect, size, font)."""
    for block in page.get_text("rawdict")["blocks"]:
        for line in block.get("lines", []):
            chars = []
            for span in line.get("spans", []):
                for c in span.get("chars", []):
                    chars.append((c["c"], fitz.Rect(c["bbox"]), span["size"], span["font"]))
            if chars:
                yield chars


def main(src: str, dst: str) -> int:
    doc = fitz.open(src)
    counts = dict.fromkeys(DEFS, 0)
    for page in doc:
        hits = []
        for chars in _line_chars(page):
            text = "".join(c for c, _, _, _ in chars)
            for tok, defn in DEFS.items():
                sans = ord(tok[0]) in _SANS
                # Only letter-like tokens can sit "inside" a longer math word;
                # operators (∘ ‖ ⊥ ₳) legitimately neighbour letters.
                blockable = ord(tok[0]) in _LATIN_ITALIC or sans or 0x1D6FC <= ord(tok[0]) <= 0x1D71B
                start = 0
                while (i := text.find(tok, start)) != -1:
                    start = i + len(tok)
                    _, _, size, font = chars[i]
                    if "JuliaMono" in font:  # code blocks: not the prose layer
                        continue
                    before = chars[i - 1] if i > 0 else None
                    after = chars[i + len(tok)] if i + len(tok) < len(chars) else None
                    if blockable and before and _is_blocking(before[0], before[2], size, sans):
                        continue
                    if blockable and after and _is_blocking(after[0], after[2], size, sans):
                        continue
                    rect = fitz.Rect(chars[i][1])
                    for _, r, _, _ in chars[i + 1 : i + len(tok)]:
                        rect |= r
                    hits.append((rect, tok, defn))
        for rect, tok, defn in hits:
            annot = page.add_highlight_annot(rect)
            annot.set_info(title="notation", content=defn)
            annot.set_opacity(0)
            annot.update()
            counts[tok] += 1
    total = sum(counts.values())
    for tok, n in counts.items():
        flag = "" if n else "   <- NO MATCHES (codepoint drift after a typst change?)"
        print(f"  {n:5d}  {tok}{flag}")
    if total == 0:
        print("annotate-notation: ERROR: no symbol matched at all", file=sys.stderr)
        return 1
    doc.save(dst)
    print(f"annotate-notation: stamped {total} tooltips over {len(DEFS)} symbols -> {dst}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1], sys.argv[2]))
