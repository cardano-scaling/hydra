#! /usr/bin/env python3

# Diff two end-to-end benchmark reports (master vs PR) and render a colored
# markdown comment. Parses the per-scenario tables emitted by
# hydra-cluster/bench/Bench/Summary.hs (formattedSummary); see that function
# for the source-of-truth row format. Pure stdlib so CI needs no extra deps.

import argparse
import re
import sys

# Metrics we diff, in display order. Each entry maps the markdown row key (the
# text between underscores in `| _key_ | value |`) to a display label and the
# direction that counts as an improvement: +1 = higher is better, -1 = lower is
# better. Keys must match Summary.hs exactly.
METRICS = [
    ("End-to-end TPS", "End-to-end TPS (tx/s)", +1),
    ("Per-snapshot TPS P50", "Per-snapshot TPS P50 (tx/s)", +1),
    ("Per-snapshot TPS P95", "Per-snapshot TPS P95 (tx/s)", +1),
    ("Per-snapshot TPS max", "Per-snapshot TPS max (tx/s)", +1),
    ("Avg. Confirmation Time (ms)", "Avg. Confirmation Time (ms)", -1),
    ("P50", "P50 confirmation (ms)", -1),
    ("P95", "P95 confirmation (ms)", -1),
    ("P99", "P99 confirmation (ms)", -1),
    ("Incremental commit avg (ms)", "Incremental commit avg (ms)", -1),
    ("Incremental decommit avg (ms)", "Incremental decommit avg (ms)", -1),
    ("Number of Invalid txs", "Invalid txs", -1),
]

# `| _key_ | value |`, tolerant of surrounding whitespace.
ROW_RE = re.compile(r"^\|\s*_(?P<key>.+?)_\s*\|\s*(?P<val>.*?)\s*\|")
NUM_RE = re.compile(r"-?\d+(?:\.\d+)?")


def parse_value(raw):
    # Strip units (ms, tx/s, ₳, %, commas) and read the first number.
    m = NUM_RE.search(raw.replace(",", ""))
    return float(m.group()) if m else None


def parse_report(path):
    """{scenario_title: {metric_key: float}} in file order."""
    scenarios = {}
    order = []
    title = None
    with open(path, encoding="utf-8") as f:
        for line in f:
            if line.startswith("## "):
                title = line[3:].strip()
                if title not in scenarios:
                    scenarios[title] = {}
                    order.append(title)
                continue
            if title is None:
                continue
            m = ROW_RE.match(line)
            if not m:
                continue
            val = parse_value(m.group("val"))
            if val is not None:
                scenarios[title][m.group("key").strip()] = val
    return scenarios, order


def fmt_num(x):
    return f"{x:.2f}"


def colored(body, delta, good_dir):
    improved = (delta > 0 and good_dir > 0) or (delta < 0 and good_dir < 0)
    color = "green" if improved else "red"
    # GitHub renders colored text via $$\color{...}$$ math. Wrap the body in
    # \text{} so spaces/parens render as text, and escape '%' as '\%' since a
    # bare '%' starts a comment in math mode and would swallow the closing
    # braces (rendering error "missing close brace").
    safe = body.replace("%", r"\%")
    return "$${\\color{" + color + "}\\text{" + safe + "}}$$"


def fmt_delta(delta, pct, good_dir, threshold):
    sign = "+" if delta >= 0 else ""
    pct_sign = "+" if pct >= 0 else ""
    body = f"{sign}{delta:.2f} ({pct_sign}{pct:.1f}%)"
    if abs(pct) < threshold:
        return f"≈ {body}"
    return colored(body, delta, good_dir)


def scenario_rows(old, new, threshold, warn_keys):
    rows = []
    for key, label, good_dir in METRICS:
        if key not in old or key not in new:
            if key in old or key in new:
                warn_keys.add(key)  # present one side only: possible format drift
            continue
        o, n = old[key], new[key]
        delta = n - o
        if o == 0:
            # No baseline to compute a percentage from. Any nonzero change is a
            # real one (the metric went from "none" to "some"), so color it.
            sign = "+" if delta >= 0 else ""
            body = f"{sign}{delta:.2f} (n/a%)"
            cell = f"≈ {body}" if delta == 0 else colored(body, delta, good_dir)
        else:
            pct = 100.0 * delta / o
            cell = fmt_delta(delta, pct, good_dir, threshold)
        rows.append(f"| {label} | {fmt_num(o)} | {fmt_num(n)} | {cell} |")
    return rows


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("old_file", help="master end-to-end-benchmarks.md")
    parser.add_argument("new_file", help="PR end-to-end-benchmarks.md")
    parser.add_argument("--threshold", type=float, default=5.0,
                        help="percent change below which a metric is treated as noise")
    args = parser.parse_args()

    base, _ = parse_report(args.old_file)
    branch, branch_order = parse_report(args.new_file)

    out = ["# End-to-end benchmark differences", ""]
    out.append(
        f"Comparing this PR (`new`) against `master` (`old`). Numbers come from "
        f"cloud VMs, so changes under {args.threshold:g}% are shown as `≈` and "
        f"are likely run-to-run noise rather than a real regression or "
        f"improvement. Green = improvement, red = regression."
    )
    out.append("")

    # Match by scenario title; fall back to positional pairing when titles are
    # absent on either side.
    common = [t for t in branch_order if t in base]
    matched_any = False
    warn_keys = set()
    for title in common:
        rows = scenario_rows(base[title], branch[title], args.threshold, warn_keys)
        if not rows:
            continue
        matched_any = True
        out += ["", f"## {title}", "",
                "| Metric | master | PR | Δ |",
                "| -- | -- | -- | -- |"]
        out += rows

    if not matched_any:
        out.append("No comparable scenarios found between this PR and `master`.")

    if warn_keys:
        # Surface format drift loudly so it's noticed in CI logs, but don't fail.
        print("WARNING: metric(s) found on only one side, skipped: "
              + ", ".join(sorted(warn_keys)), file=sys.stderr)

    print("\n".join(out))


if __name__ == "__main__":
    main()
