#!/usr/bin/env python3
"""Sort the PDF named-destination name tree that Typst emits unsorted.

Typst 0.14 (through at least 0.14.2) writes the catalog's /Names -> /Dests
-> /Names array in label-insertion order. ISO 32000-1 (7.9.6, "Name Trees")
requires the keys to be sorted lexically; viewers that binary-search the
tree per spec (Chromium/PDFium, macOS Quartz/Preview, pdf.js) then fail to
resolve most names, so internal links targeting named destinations (all
section links) silently do nothing there. Poppler-based viewers (okular,
evince) tolerate the unsorted tree. Upstream fixed this identically in
krilla#304 (for typst/typst#7248), not yet in a 0.14.x release; delete this
step once the Typst in use ships it. This rewrites the array in sorted
order, changing nothing else.
"""

import sys

import pikepdf


def main(path: str) -> int:
    with pikepdf.open(path, allow_overwriting_input=True) as pdf:
        names = pdf.Root.get("/Names")
        dests = names.get("/Dests") if names is not None else None
        if dests is None:
            return 0  # no named destinations, nothing to do
        if "/Kids" in dests:
            print(
                f"{path}: /Dests name tree has /Kids nodes; this script only "
                "handles the flat single-node tree Typst emits",
                file=sys.stderr,
            )
            return 1
        arr = dests.Names
        pairs = [(bytes(arr[i]), arr[i], arr[i + 1]) for i in range(0, len(arr), 2)]
        pairs.sort(key=lambda t: t[0])
        flat = [x for _, k, v in pairs for x in (k, v)]
        dests.Names = pikepdf.Array(flat)
        pdf.save(path)
    print(f"sort-named-dests: sorted {len(pairs)} named destinations in {path}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1]))
