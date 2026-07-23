#! /usr/bin/env python3

# Diff two end-to-end benchmark reports (master vs PR) and render a colored
# markdown comment. Parses the per-scenario tables emitted by
# hydra-cluster/bench/Bench/Summary.hs (formattedSummary); see that function
# for the source-of-truth row format. Pure stdlib so CI needs no extra deps.

import argparse
import re
import sys

# Metrics we diff, in display order. Each entry is (row key, display label,
# improvement direction, display scale):
#   - row key: the text between underscores in `| _key_ | value |`, matched
#     against Summary.hs's output exactly (same key in both the master and PR
#     reports).
#   - improvement direction: +1 = higher is better, -1 = lower is better,
#     0 = neutral. Neutral rows are shown without a good/bad color: e.g.
#     snapshot counts move with the node's snapshot batching cap without being
#     better or worse by themselves (throughput = snapshots/s x txs/snapshot).
#   - display scale: multiplies the raw value before rendering. Summary.hs emits
#     the confirmation/validation rows in milliseconds, but under load they run
#     to tens of thousands of ms; they read more naturally in seconds, so they
#     are scaled by MS_TO_S and relabelled "(s)". Both reports are scaled the
#     same way, so the delta and percentage are unchanged.
# Keys must match Summary.hs exactly.
MS_TO_S = 1e-3
METRICS = [
    ("End-to-end TPS", "End-to-end TPS (tx/s)", +1, 1.0),
    ("Sustained TPS", "Sustained TPS (tx/s)", +1, 1.0),
    ("Backlog drain time (s)", "Backlog drain time (s)", -1, 1.0),
    ("Snapshots per second", "Snapshots per second (/s)", 0, 1.0),
    ("Avg txs per snapshot", "Avg txs per snapshot", 0, 1.0),
    ("Avg. Confirmation Time (ms)", "Avg. Confirmation Time (s)", -1, MS_TO_S),
    ("P50", "P50 confirmation (s)", -1, MS_TO_S),
    ("P95", "P95 confirmation (s)", -1, MS_TO_S),
    ("P99", "P99 confirmation (s)", -1, MS_TO_S),
    ("Tx validation time p50 (ms)", "Tx validation time p50 (s)", -1, MS_TO_S),
    ("Peak node RSS (MB)", "Peak node RSS (MB)", 0, 1.0),
    ("Incremental commit avg (ms)", "Incremental commit avg (s)", -1, MS_TO_S),
    ("Incremental decommit avg (ms)", "Incremental decommit avg (s)", -1, MS_TO_S),
    ("Number of Invalid txs", "Invalid txs", -1, 1.0),
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


def fmt_num(x, decimals=2):
    return f"{x:,.{decimals}f}"


def colored(body, delta, good_dir):
    # We deliberately avoid GitHub's $$\color{...}$$ math trick: a literal '%'
    # in math mode is a comment and GitHub's markdown pipeline mangles even an
    # escaped '\%', truncating the cell. A colored emoji + plain text is robust
    # everywhere (including mobile) and conveys the same green/red signal.
    improved = (delta > 0 and good_dir > 0) or (delta < 0 and good_dir < 0)
    return f"{'🟢' if improved else '🔴'} {body}"


def fmt_delta(delta, pct, good_dir, threshold, decimals=2):
    sign = "+" if delta >= 0 else ""
    pct_sign = "+" if pct >= 0 else ""
    body = f"{sign}{delta:,.{decimals}f} ({pct_sign}{pct:.1f}%)"
    if abs(pct) < threshold:
        return f"≈ {body}"
    if good_dir == 0:
        return body
    return colored(body, delta, good_dir)


# Headline metrics that emit a GitHub ::warning:: annotation (still exit 0)
# when they regress more than WARN_PCT. Coarse on purpose: shared-runner e2e
# noise makes a hard gate impractical, but a large drop on a headline rate
# should be visible without opening the report. Only directional metrics
# belong here; neutral (0) ones have no regression direction.
WARN_METRICS = {"End-to-end TPS", "Sustained TPS"}
WARN_PCT = 15.0


def scenario_rows(old, new, threshold, warn_keys, regressions, title):
    rows = []
    for key, label, good_dir, scale in METRICS:
        if key not in old or key not in new:
            if key in old or key in new:
                warn_keys.add(key)  # present one side only: possible format drift
            continue
        o, n = old[key] * scale, new[key] * scale
        # The ms->s scaled rows keep millisecond resolution (3 decimals) so the
        # sub-second closed-loop latencies stay meaningful; the throughput and
        # RSS rows read fine at 2.
        decimals = 3 if scale != 1.0 else 2
        delta = n - o
        if key in WARN_METRICS and o != 0:
            pct = 100.0 * delta * good_dir / o
            if pct < -WARN_PCT:
                regressions.append(f"{title}: {label} changed {100.0 * delta / o:+.1f}%")
        if o == 0:
            # No baseline to compute a percentage from. Any nonzero change is a
            # real one (the metric went from "none" to "some"), so color it.
            sign = "+" if delta >= 0 else ""
            body = f"{sign}{delta:,.{decimals}f} (n/a%)"
            if delta == 0:
                cell = f"≈ {body}"
            elif good_dir == 0:
                cell = body
            else:
                cell = colored(body, delta, good_dir)
        else:
            pct = 100.0 * delta / o
            cell = fmt_delta(delta, pct, good_dir, threshold, decimals)
        rows.append(f"| {label} | {fmt_num(o, decimals)} | {fmt_num(n, decimals)} | {cell} |")
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
        f"improvement. 🟢 = improvement, 🔴 = regression; uncolored rows are "
        f"neutral measures reported for context."
    )
    out.append("")

    # Match scenarios by title (reports always carry one; untitled datasets
    # get a default title from the bench).
    common = [t for t in branch_order if t in base]
    matched_any = False
    warn_keys = set()
    regressions = []
    for title in common:
        rows = scenario_rows(base[title], branch[title], args.threshold, warn_keys, regressions, title)
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

    for regression in regressions:
        # GitHub annotation; soft gate only (exit code stays 0).
        print(f"::warning title=Benchmark regression::{regression}", file=sys.stderr)

    print("\n".join(out))


if __name__ == "__main__":
    main()
