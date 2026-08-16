#! /usr/bin/env python3

# Diff end-to-end benchmark results (master vs PR) and render a colored
# markdown comment. Pure stdlib so CI needs no extra deps.
#
# Two input modes:
#
#   bench-e2e-diff.py old.md new.md
#     Legacy: one report per side, one implicit pair. Kept for humans and
#     BASELINES-style comparisons.
#
#   bench-e2e-diff.py --results-dir DIR
#     Paired mode, produced by .github/workflows/bench-e2e-diff.yaml. Layout:
#       DIR/<machine>/fingerprint.json                        (optional)
#       DIR/<machine>/rep-<slot>-<side>/**/end-to-end-benchmarks.{json,md}
#     Every benchmark job measures BOTH sides on its own runner, so adjacent
#     slots on one machine form a same-machine pair and machine identity
#     cancels in the per-pair delta. GitHub's runner fleet spans CPU models
#     with ~68% single-thread spread, so unpaired cross-machine comparisons
#     are dominated by which VMs the jobs landed on, not by the code.
#
# Aggregation (a calibrated heuristic, not a significance test): per metric,
# compute the percent delta of each valid pair; report the median and the
# min..max pair spread; color only when |median| exceeds the per-metric
# threshold (scripts/bench-e2e-thresholds.json) AND at least ceil(0.75*n)
# pairs agree with the median's sign. "Strong" (bolded, plus a ::warning
# annotation for headline metrics) needs all pairs agreeing and 2x threshold.
#
# Reports carry raw series (snapshot series, confirmation times) in their JSON
# twin; derived estimators are computed HERE, with one implementation for both
# sides, because each side runs its own bench binary and a PR changing an
# estimator definition in Haskell would otherwise compare two differently
# defined metrics on its own PR. Pairs whose sides have different formats
# (json vs md-only) are skipped rather than mixing estimator definitions.

import argparse
import json
import math
import re
import sys
from pathlib import Path

MS_TO_S = 1e-3
OPEN, CLOSED = "open-loop", "closed-loop"
BOTH = {OPEN, CLOSED}

# Metrics we diff, in display order:
#   - key: row key in `| _key_ | value |` (Summary.hs formattedSummary) and
#     equally the key of the record derived from the JSON twin.
#   - direction: +1 higher is better, -1 lower is better, 0 neutral context.
#   - display scale: ms rows read better in seconds; both sides are scaled the
#     same way so deltas are unchanged.
#   - modes: which load modes show the row. Open-loop scenarios run saturated,
#     so their confirmation-latency rows restate throughput (see
#     hydra-cluster/README.md) and are context at best; they stay in the md
#     report but not in this diff. P99 is dropped everywhere: confirmations
#     arrive in per-snapshot bursts, so the top percentile is a handful of
#     atoms and degenerate.
#   - kind: "count" rows diff by absolute delta (a percentage of zero baseline
#     is undefined); everything else by percent.
METRICS = [
    ("End-to-end TPS", "End-to-end TPS (tx/s)", +1, 1.0, BOTH, "pct"),
    ("Sustained TPS", "Sustained TPS (tx/s)", +1, 1.0, BOTH, "pct"),
    ("Sustained TPS (slope)", "Sustained TPS, slope (tx/s)", +1, 1.0, BOTH, "pct"),
    ("Backlog drain time (s)", "Backlog drain time (s)", -1, 1.0, {OPEN}, "pct"),
    ("Snapshots observed", "Snapshots observed", 0, 1.0, BOTH, "pct"),
    ("Snapshots per second", "Snapshots per second (/s)", 0, 1.0, BOTH, "pct"),
    ("Avg txs per snapshot", "Avg txs per snapshot", 0, 1.0, BOTH, "pct"),
    ("Avg. Confirmation Time (ms)", "Avg. Confirmation Time (s)", -1, MS_TO_S, {CLOSED}, "pct"),
    ("P50", "P50 confirmation (s)", -1, MS_TO_S, {CLOSED}, "pct"),
    ("P95", "P95 confirmation (s)", -1, MS_TO_S, {CLOSED}, "pct"),
    ("Tx validation time p50 (ms)", "Tx validation time p50 (s)", -1, MS_TO_S, BOTH, "pct"),
    ("Alloc MB per confirmed tx", "Alloc MB per confirmed tx", -1, 1.0, BOTH, "pct"),
    ("Alloc MB per snapshot", "Alloc MB per snapshot", 0, 1.0, BOTH, "pct"),
    ("Mutator CPU s per 1k txs", "Mutator CPU s per 1k txs", -1, 1.0, BOTH, "pct"),
    ("Max live MB (max node)", "Max live MB (max node)", -1, 1.0, BOTH, "pct"),
    ("Peak node RSS (MB)", "Peak node RSS (MB)", -1, 1.0, BOTH, "pct"),
    ("Number of Invalid txs", "Invalid txs", -1, 1.0, BOTH, "count"),
    ("Incremental commit avg (ms)", "Incremental commit avg (s)", -1, MS_TO_S, BOTH, "pct"),
    ("Incremental decommit avg (ms)", "Incremental decommit avg (s)", -1, MS_TO_S, BOTH, "pct"),
]

# Fallback when scripts/bench-e2e-thresholds.json is missing. Initial values
# are hypotheses; the calibration loop (--calibrate over nightly A/A results)
# replaces them with the observed p95 of |median pair delta| per metric.
DEFAULT_THRESHOLDS = {
    "default": 10.0,
    "metrics": {
        "End-to-end TPS": 10.0,
        "Sustained TPS": 10.0,
        "Sustained TPS (slope)": 10.0,
        "Backlog drain time (s)": 10.0,
        "Avg. Confirmation Time (ms)": 5.0,
        "P50": 5.0,
        "P95": 5.0,
        "Tx validation time p50 (ms)": 10.0,
        "Alloc MB per confirmed tx": 5.0,
        "Mutator CPU s per 1k txs": 8.0,
        "Max live MB (max node)": 10.0,
        "Peak node RSS (MB)": 10.0,
    },
}

# Headline metrics that emit a GitHub ::warning:: annotation (still exit 0)
# on a strong regression. The counter metrics are the machine-insensitive
# signal intended to become a hard gate once A/A calibration proves them.
WARN_METRICS = {
    "End-to-end TPS",
    "Sustained TPS",
    "Sustained TPS (slope)",
    "Alloc MB per confirmed tx",
    "Mutator CPU s per 1k txs",
}

REQUIRED_AGREEMENT = 0.75

# `| _key_ | value |`, tolerant of surrounding whitespace.
ROW_RE = re.compile(r"^\|\s*_(?P<key>.+?)_\s*\|\s*(?P<val>.*?)\s*\|")
NUM_RE = re.compile(r"-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?")
REP_DIR_RE = re.compile(r"^rep-(?P<slot>\d+)-(?P<side>branch|master)$")


def load_thresholds():
    path = Path(__file__).resolve().parent / "bench-e2e-thresholds.json"
    try:
        with open(path, encoding="utf-8") as f:
            return json.load(f)
    except (OSError, ValueError):
        return DEFAULT_THRESHOLDS


def threshold_for(thresholds, key):
    return float(thresholds.get("metrics", {}).get(key, thresholds.get("default", 10.0)))


def parse_value(raw):
    # Strip units (ms, tx/s, %, commas) and read the first number.
    m = NUM_RE.search(raw.replace(",", ""))
    return float(m.group()) if m else None


def percentile(sorted_vals, p):
    # Linear interpolation between closest ranks; sorted_vals ascending.
    if not sorted_vals:
        return None
    k = (len(sorted_vals) - 1) * p / 100.0
    f, c = math.floor(k), math.ceil(k)
    if f == c:
        return sorted_vals[int(k)]
    return sorted_vals[f] * (c - k) + sorted_vals[c] * (k - f)


def sustained_tps_slope(snapshot_series):
    """Least-squares slope of cumulative confirmed txs over time, restricted
    to snapshot points whose cumulative count lies in the middle 80%. Robust
    to snapshot-boundary granularity, unlike endpoint-based trimming; needs
    >= 4 points in the window."""
    pts, cum = [], 0
    for t, n in sorted((float(t), int(n)) for t, n in snapshot_series):
        cum += n
        pts.append((t, cum))
    total = cum
    if total <= 0:
        return None
    lo, hi = 0.10 * total, 0.90 * total
    window = [(t, c) for t, c in pts if lo <= c <= hi]
    if len(window) < 4:
        return None
    n = len(window)
    mean_t = sum(t for t, _ in window) / n
    mean_c = sum(c for _, c in window) / n
    denom = sum((t - mean_t) ** 2 for t, _ in window)
    if denom <= 0:
        return None
    return sum((t - mean_t) * (c - mean_c) for t, c in window) / denom


def rts_metrics(node_stats, n_txs, n_snapshots):
    # Mirrors Bench.Summary.rtsAggregates so json- and md-sourced records
    # carry identical values.
    if not node_stats or n_txs <= 0 or n_snapshots <= 0:
        return {}
    mb = 1024.0 * 1024.0
    total_alloc_mb = sum(s["allocatedBytes"] for s in node_stats) / mb
    total_mut_cpu = sum(s["mutatorCpuSeconds"] for s in node_stats)
    return {
        "Alloc MB per confirmed tx": total_alloc_mb / n_txs,
        "Alloc MB per snapshot": total_alloc_mb / n_snapshots,
        "Mutator CPU s per 1k txs": total_mut_cpu / (n_txs / 1000.0),
        "Max live MB (max node)": max(s["maxLiveBytes"] for s in node_stats) / mb,
    }


def summary_to_record(s):
    """One JSON summary -> internal record with the same metric keys the md
    rows use, estimators recomputed from the raw series."""
    metrics = {}
    n_txs = s.get("numberOfTxs") or 0
    wall = s.get("runWallClockSeconds") or 0.0
    if n_txs:
        metrics["Number of txs"] = float(n_txs)
    conf_ms = s.get("confirmationTimesMs") or []
    if conf_ms:
        metrics["Avg. Confirmation Time (ms)"] = sum(conf_ms) / len(conf_ms)
        metrics["P50"] = percentile(conf_ms, 50)
        metrics["P95"] = percentile(conf_ms, 95)
    if s.get("validationP50Ms") is not None:
        metrics["Tx validation time p50 (ms)"] = s["validationP50Ms"]
    if s.get("endToEndTps") is not None:
        metrics["End-to-end TPS"] = s["endToEndTps"]
        if wall > 0 and n_txs:
            recomputed = n_txs / wall
            if s["endToEndTps"] and abs(recomputed - s["endToEndTps"]) > 0.01 * s["endToEndTps"]:
                print(
                    f"WARNING: reported End-to-end TPS {s['endToEndTps']:.2f} deviates "
                    f">1% from recomputed {recomputed:.2f} ({s.get('summaryTitle')})",
                    file=sys.stderr,
                )
    slope = sustained_tps_slope(s.get("snapshotSeries") or [])
    if slope is not None:
        metrics["Sustained TPS (slope)"] = slope
    metrics["Backlog drain time (s)"] = s.get("drainSeconds") or 0.0
    n_snapshots = s.get("numberOfSnapshots") or 0
    metrics["Snapshots observed"] = float(n_snapshots)
    if wall > 0:
        metrics["Snapshots per second"] = n_snapshots / wall
    metrics["Avg txs per snapshot"] = s.get("avgTxsPerSnapshot") or 0.0
    if s.get("peakNodeRssMb") is not None:
        metrics["Peak node RSS (MB)"] = s["peakNodeRssMb"]
    metrics["Number of Invalid txs"] = float(s.get("numberOfInvalidTxs") or 0)
    metrics.update(rts_metrics(s.get("nodeRtsStats") or [], n_txs, n_snapshots))
    for field, key in [
        ("incrementalCommitTimes", "Incremental commit avg (ms)"),
        ("incrementalDecommitTimes", "Incremental decommit avg (ms)"),
    ]:
        times = s.get(field) or []
        if times:
            metrics[key] = 1000.0 * sum(times) / len(times)
    outcome = s.get("runOutcome")
    return {
        "title": s.get("summaryTitle") or "Baseline Scenario",
        "load_mode": s.get("loadMode"),
        "outcome": outcome,
        "failed": outcome is not None or n_txs == 0,
        "metrics": metrics,
        "source": "json",
    }


def parse_json_report(path):
    with open(path, encoding="utf-8") as f:
        doc = json.load(f)
    records = {}
    order = []
    for s in doc.get("summaries", []):
        rec = summary_to_record(s)
        if rec["title"] not in records:
            records[rec["title"]] = rec
            order.append(rec["title"])
    return records, order


def parse_md_report(path):
    """{scenario_title: record} in file order, from the markdown table rows."""
    records = {}
    order = []
    rec = None
    with open(path, encoding="utf-8") as f:
        for line in f:
            if line.startswith("## "):
                title = line[3:].strip()
                if title not in records:
                    rec = {
                        "title": title,
                        "load_mode": None,
                        "outcome": None,
                        "failed": False,
                        "metrics": {},
                        "source": "md",
                    }
                    records[title] = rec
                    order.append(title)
                else:
                    rec = records[title]
                continue
            if rec is None:
                continue
            m = ROW_RE.match(line)
            if not m:
                continue
            key, raw = m.group("key").strip(), m.group("val").strip()
            if key == "Load mode":
                rec["load_mode"] = raw
                continue
            if key == "Outcome":
                rec["outcome"] = raw
                rec["failed"] = True
                continue
            val = parse_value(raw)
            if val is not None:
                rec["metrics"][key] = val
    for rec in records.values():
        if "Number of txs" not in rec["metrics"]:
            rec["failed"] = True
            rec["outcome"] = rec["outcome"] or "did not complete, no measurements"
    return records, order


def parse_report_any(json_path, md_path):
    if json_path is not None and json_path.exists():
        try:
            return parse_json_report(json_path)
        except (ValueError, KeyError) as e:
            print(f"WARNING: unreadable JSON report {json_path}: {e}", file=sys.stderr)
    if md_path is not None and md_path.exists():
        return parse_md_report(md_path)
    return {}, []


def collect_results(results_dir):
    """DIR/<machine>/rep-<slot>-<side>/**/end-to-end-benchmarks.{json,md}
    -> (machines, scenario_order). Reps merge scenarios from every report
    under their directory (open and closed invocations write separately)."""
    machines = {}
    scenario_order = []
    root = Path(results_dir)
    for machine_dir in sorted(p for p in root.iterdir() if p.is_dir()):
        machine = {"name": machine_dir.name, "fingerprint": None, "reps": []}
        for fp in sorted(machine_dir.glob("**/fingerprint.json")):
            try:
                with open(fp, encoding="utf-8") as f:
                    machine["fingerprint"] = json.load(f)
                break
            except (OSError, ValueError):
                pass
        for rep_dir in sorted(p for p in machine_dir.iterdir() if p.is_dir()):
            m = REP_DIR_RE.match(rep_dir.name)
            if not m:
                continue
            scenarios = {}
            for md in sorted(rep_dir.glob("**/end-to-end-benchmarks.md")):
                recs, order = parse_report_any(md.with_suffix(".json"), md)
                for t in order:
                    if t not in scenarios:
                        scenarios[t] = recs[t]
                        if t not in scenario_order:
                            scenario_order.append(t)
            # JSON-only rep dirs (md missing for whatever reason).
            for js in sorted(rep_dir.glob("**/end-to-end-benchmarks.json")):
                if js.with_suffix(".md").exists():
                    continue
                recs, order = parse_report_any(js, None)
                for t in order:
                    if t not in scenarios:
                        scenarios[t] = recs[t]
                        if t not in scenario_order:
                            scenario_order.append(t)
            machine["reps"].append(
                {"slot": int(m.group("slot")), "side": m.group("side"), "scenarios": scenarios}
            )
        machine["reps"].sort(key=lambda r: r["slot"])
        if machine["reps"]:
            machines[machine_dir.name] = machine
    return machines, scenario_order


def build_pairs(machines):
    """Adjacent slots on one machine form a pair; each must contain one side
    each. Slot orders alternate across machines (AB/BA) so slot-position
    effects cancel in the median."""
    pairs = []
    for machine in machines.values():
        reps = machine["reps"]
        for i in range(0, len(reps) - 1, 2):
            a, b = reps[i], reps[i + 1]
            if {a["side"], b["side"]} != {"branch", "master"}:
                print(
                    f"WARNING: slots {a['slot']},{b['slot']} on {machine['name']} do not "
                    f"form a branch/master pair; skipped",
                    file=sys.stderr,
                )
                continue
            old = a if a["side"] == "master" else b
            new = a if a["side"] == "branch" else b
            pairs.append({"machine": machine["name"], "old": old, "new": new})
    return pairs


def fmt_num(x, decimals=2):
    return f"{x:,.{decimals}f}"


def median(xs):
    ys = sorted(xs)
    n = len(ys)
    mid = n // 2
    return ys[mid] if n % 2 else (ys[mid - 1] + ys[mid]) / 2.0


def sign(x):
    return (x > 0) - (x < 0)


def decide_cell(deltas, direction, threshold, kind):
    """Aggregate per-pair deltas into (cell text, strong_regression flag,
    median). `deltas` are percent for kind 'pct', absolute for 'count'."""
    n = len(deltas)
    med = median(deltas)
    agreeing = sum(1 for d in deltas if sign(d) == sign(med))
    required = math.ceil(REQUIRED_AGREEMENT * n)
    spread = f" [{min(deltas):+.1f} .. {max(deltas):+.1f}]" if n > 1 else ""
    count = f" ({agreeing}/{n})" if n > 1 else ""
    unit = "%" if kind == "pct" else ""
    body = f"{med:+.1f}{unit}{spread}{count}"
    if kind == "count":
        significant = med != 0
        strong = significant and agreeing == n
    else:
        significant = abs(med) >= threshold and agreeing >= required
        strong = abs(med) >= 2 * threshold and agreeing == n
    if not significant:
        # Flag disagreement when at least one pair moved beyond the threshold
        # but the pairs do not concur: big conflicting swings are noise worth
        # noticing, small ones are just noise.
        conflicted = any(abs(d) >= threshold for d in deltas) and agreeing < required
        note = ", pairs disagree" if conflicted else ""
        return f"≈ {body}{note}", False, med
    if direction == 0:
        return body, False, med
    improved = (med > 0 and direction > 0) or (med < 0 and direction < 0)
    emoji = "🟢" if improved else "🔴"
    shown = f"**{body}**" if strong else body
    return f"{emoji} {shown}", (strong and not improved), med


def scenario_mode(recs):
    for rec in recs:
        if rec and rec.get("load_mode") in (OPEN, CLOSED):
            return rec["load_mode"]
    for rec in recs:
        if rec and "closed-loop" in rec["title"]:
            return CLOSED
    return None


def scenario_rows(title, pairs, thresholds, regressions):
    """Render one scenario's table rows from its valid pairs; returns (rows,
    n_valid, notes)."""
    valid = []
    format_skips = 0
    for p in pairs:
        old = p["old"]["scenarios"].get(title)
        new = p["new"]["scenarios"].get(title)
        if old is None or new is None or old["failed"] or new["failed"]:
            continue
        if old["source"] != new["source"]:
            format_skips += 1
            continue
        valid.append((old, new))
    if not valid:
        return [], 0, format_skips
    mode = scenario_mode([new for _, new in valid] + [old for old, _ in valid])
    rows = []
    for key, label, direction, scale, modes, kind in METRICS:
        if mode is not None and mode not in modes:
            continue
        deltas, olds, news = [], [], []
        for old, new in valid:
            if key not in old["metrics"] or key not in new["metrics"]:
                continue
            o, n = old["metrics"][key] * scale, new["metrics"][key] * scale
            olds.append(o)
            news.append(n)
            if kind == "count":
                deltas.append(n - o)
            elif o != 0:
                deltas.append(100.0 * (n - o) / o)
        if not deltas:
            continue
        decimals = 3 if scale != 1.0 else 2
        threshold = threshold_for(thresholds, key)
        cell, strong_regression, med = decide_cell(deltas, direction, threshold, kind)
        if len(deltas) < len(valid):
            cell += f" ({len(deltas)}/{len(valid)} pairs)"
        if strong_regression and key in WARN_METRICS:
            regressions.append(f"{title}: {label} changed {med:+.1f}%")
        rows.append(
            f"| {label} | {fmt_num(median(olds), decimals)} | {fmt_num(median(news), decimals)} | {cell} |"
        )
    return rows, len(valid), format_skips


def failed_sides(title, pairs):
    """Per side: (failed reps, total reps, first reason) for this scenario."""
    out = {}
    for side in ("old", "new"):
        total, failed, reason = 0, 0, None
        for p in pairs:
            rec = p[side]["scenarios"].get(title)
            if rec is None:
                continue
            total += 1
            if rec["failed"]:
                failed += 1
                reason = reason or rec["outcome"]
        out[side] = (failed, total, reason)
    return out


def same_code_spread(machines, title, key="End-to-end TPS"):
    """Cross-machine spread on identical code, per side: the noise floor an
    unpaired comparison would be exposed to."""
    out = {}
    for side in ("master", "branch"):
        vals = []
        for machine in machines.values():
            for rep in machine["reps"]:
                if rep["side"] != side:
                    continue
                rec = rep["scenarios"].get(title)
                if rec and not rec["failed"] and key in rec["metrics"]:
                    vals.append(rec["metrics"][key])
        if len(vals) >= 2 and min(vals) > 0:
            out[side] = 100.0 * (max(vals) - min(vals)) / min(vals)
    return out


def render_details(machines, scenario_order):
    lines = ["", "<details>", "<summary>Per-run raw values</summary>", ""]
    lines += ["| Machine | Slot | Side | Scenario | E2E TPS | Snapshots | Outcome |", "| -- | -- | -- | -- | -- | -- | -- |"]
    for machine in machines.values():
        for rep in machine["reps"]:
            for title in scenario_order:
                rec = rep["scenarios"].get(title)
                if rec is None:
                    continue
                tps = rec["metrics"].get("End-to-end TPS")
                snaps = rec["metrics"].get("Snapshots observed")
                outcome = rec["outcome"] or "ok"
                lines.append(
                    f"| {machine['name']} | {rep['slot']} | {rep['side']} | {title} | "
                    f"{fmt_num(tps) if tps is not None else 'n/a'} | "
                    f"{int(snaps) if snaps is not None else 'n/a'} | {outcome} |"
                )
    lines += ["", "</details>"]
    return lines


def render_paired(machines, scenario_order, args, thresholds):
    pairs = build_pairs(machines)
    out = ["# End-to-end benchmark differences", ""]
    shas = ""
    if args.head_sha or args.base_sha:
        shas = (
            f"Comparing `{(args.head_sha or 'PR')[:7]}` (PR) against merge-base "
            f"`{(args.base_sha or 'master')[:7]}`. "
        )
    out.append(
        shas
        + f"Design: {len(machines)} runner(s), each measuring both sides "
        + "(orders alternated); every delta below aggregates same-machine pairs as "
        + "`median [min .. max] (pairs agreeing)`. Colored rows exceed the "
        + "per-metric noise threshold with directional agreement; this is a "
        + "calibrated heuristic, not a significance test. `≈` is within noise. "
        + "🟢 = improvement, 🔴 = regression; uncolored rows are context."
    )
    out.append("")

    if not pairs:
        out.append("No valid same-machine pairs found; benchmark runs likely failed. See the workflow run for logs.")
        return out, []

    regressions = []
    matched_any = False
    for title in scenario_order:
        rows, n_valid, format_skips = scenario_rows(title, pairs, thresholds, regressions)
        if n_valid == 0:
            sides = failed_sides(title, pairs)
            fo, to, ro = sides["old"]
            fn, tn, rn = sides["new"]
            out += ["", f"## {title}", ""]
            if to and tn and fo == to and fn < tn:
                out.append(f"**FAILED on master ({fo}/{to} runs)**: {ro or 'unknown'}. PR side ran; no comparison possible.")
            elif to and tn and fn == tn and fo < to:
                out.append(f"**FAILED on PR ({fn}/{tn} runs)**: {rn or 'unknown'}. Likely real breakage introduced by this PR.")
            elif format_skips:
                out.append(f"No comparable pairs: {format_skips} pair(s) skipped (report format differs between sides, expected while a bench format change is in flight).")
            else:
                out.append(f"**FAILED on both sides**: master {fo}/{to}, PR {fn}/{tn}. First reasons: {ro or 'n/a'} / {rn or 'n/a'}.")
            matched_any = True
            continue
        if not rows:
            continue
        matched_any = True
        header = f"## {title}"
        if n_valid < len(pairs):
            header += f" ({n_valid}/{len(pairs)} pairs)"
        out += ["", header, "", "| Metric | master | PR | Δ |", "| -- | -- | -- | -- |"]
        out += rows

    if not matched_any:
        out.append("No comparable scenarios found between this PR and `master`.")

    footer = ["", "---", ""]
    for name, machine in machines.items():
        fp = machine["fingerprint"] or {}
        desc = fp.get("cpu_model", "unknown CPU")
        cores = fp.get("nproc")
        mem = fp.get("mem_gb")
        extra = ", ".join(str(x) for x in [f"{cores} vCPU" if cores else None, f"{mem} GB" if mem else None] if x)
        footer.append(f"- `{name}`: {desc}{f' ({extra})' if extra else ''}")
    spreads = []
    for title in scenario_order:
        spread = same_code_spread(machines, title)
        if spread:
            parts = ", ".join(f"{side} {pct:.1f}%" for side, pct in sorted(spread.items()))
            spreads.append(f"  - {title}: {parts}")
    if spreads:
        footer.append("- Same-code spread across runners (End-to-end TPS), the noise an unpaired comparison would see:")
        footer += spreads
    out += footer
    out += render_details(machines, scenario_order)
    return out, regressions


# --- Legacy two-file mode -----------------------------------------------------


def render_legacy(old_file, new_file, threshold_override, thresholds):
    old_recs, _ = parse_report_any(None, Path(old_file))
    new_recs, new_order = parse_report_any(None, Path(new_file))
    pseudo_pairs = [
        {
            "machine": "local",
            "old": {"slot": 1, "side": "master", "scenarios": old_recs},
            "new": {"slot": 2, "side": "branch", "scenarios": new_recs},
        }
    ]
    if threshold_override is not None:
        thresholds = {"default": threshold_override, "metrics": {}}
    out = ["# End-to-end benchmark differences", ""]
    out.append(
        f"Comparing this PR (`new`) against `master` (`old`), single run per side. "
        f"Changes under the per-metric noise threshold are shown as `≈`. "
        f"🟢 = improvement, 🔴 = regression; uncolored rows are context."
    )
    regressions = []
    matched_any = False
    for title in new_order:
        if title not in old_recs:
            continue
        rows, n_valid, _ = scenario_rows(title, pseudo_pairs, thresholds, regressions)
        if n_valid == 0:
            old_rec, new_rec = old_recs.get(title), new_recs.get(title)
            out += ["", f"## {title}", ""]
            for side, rec in (("master", old_rec), ("PR", new_rec)):
                if rec is not None and rec["failed"]:
                    out.append(f"**FAILED on {side}**: {rec['outcome'] or 'unknown'}")
            matched_any = True
            continue
        if not rows:
            continue
        matched_any = True
        out += ["", f"## {title}", "", "| Metric | master | PR | Δ |", "| -- | -- | -- | -- |"]
        out += rows
    if not matched_any:
        out.append("No comparable scenarios found between this PR and `master`.")
    return out, regressions


# --- Threshold calibration ----------------------------------------------------


def calibrate(runs_dir):
    """Each subdirectory of runs_dir is one downloaded A/A results tree.
    Suggest per-metric thresholds as the p95 of |median pair delta|."""
    per_metric = {}
    runs = [p for p in Path(runs_dir).iterdir() if p.is_dir()]
    for run in sorted(runs):
        machines, scenario_order = collect_results(run)
        pairs = build_pairs(machines)
        if not pairs:
            print(f"WARNING: no pairs in {run}", file=sys.stderr)
            continue
        for title in scenario_order:
            for key, _, _, scale, _, kind in METRICS:
                if kind != "pct":
                    continue
                deltas = []
                for p in pairs:
                    old = p["old"]["scenarios"].get(title)
                    new = p["new"]["scenarios"].get(title)
                    if not old or not new or old["failed"] or new["failed"]:
                        continue
                    if key not in old["metrics"] or key not in new["metrics"]:
                        continue
                    o, n = old["metrics"][key] * scale, new["metrics"][key] * scale
                    if o != 0:
                        deltas.append(100.0 * (n - o) / o)
                if deltas:
                    per_metric.setdefault(key, []).append(abs(median(deltas)))
    suggested = {}
    for key, meds in sorted(per_metric.items()):
        meds.sort()
        idx = min(len(meds) - 1, math.ceil(0.95 * len(meds)) - 1)
        suggested[key] = round(max(meds[max(idx, 0)], 1.0), 1)
    print(json.dumps({"default": 10.0, "metrics": suggested, "_runs": len(runs)}, indent=2))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("old_file", nargs="?", help="legacy mode: master end-to-end-benchmarks.md")
    parser.add_argument("new_file", nargs="?", help="legacy mode: PR end-to-end-benchmarks.md")
    parser.add_argument("--results-dir", help="paired mode: directory of per-machine results")
    parser.add_argument("--base-sha", default=None)
    parser.add_argument("--head-sha", default=None)
    parser.add_argument("--calibrate", metavar="DIR", help="suggest thresholds from a directory of A/A results trees")
    parser.add_argument("--threshold", type=float, default=None,
                        help="legacy mode: override all noise thresholds (percent)")
    args = parser.parse_args()

    if args.calibrate:
        calibrate(args.calibrate)
        return

    thresholds = load_thresholds()
    if args.results_dir:
        machines, scenario_order = collect_results(args.results_dir)
        out, regressions = render_paired(machines, scenario_order, args, thresholds)
    elif args.old_file and args.new_file:
        out, regressions = render_legacy(args.old_file, args.new_file, args.threshold, thresholds)
    else:
        parser.error("either --results-dir, --calibrate, or two report files are required")
        return

    for regression in regressions:
        # GitHub annotation; soft gate only (exit code stays 0).
        print(f"::warning title=Benchmark regression::{regression}", file=sys.stderr)

    print("\n".join(out))


if __name__ == "__main__":
    main()
