#! /usr/bin/env python3

# Diff end-to-end benchmark reports (master vs PR) into a colored markdown
# comment. Pure stdlib. Two modes:
#
#   bench-e2e-diff.py old.md new.md         one report per side (local use)
#   bench-e2e-diff.py --results-dir DIR     paired CI mode
#
# Paired mode layout, produced by .github/workflows/bench-e2e-diff.yaml:
#   DIR/<machine>/fingerprint.json                                (optional)
#   DIR/<machine>/rep-<slot>-<side>/**/end-to-end-benchmarks.md
# Every benchmark job measures BOTH sides on its own runner, so adjacent slots
# form a same-machine pair and machine identity cancels in the pair delta
# (GitHub's fleet mixes CPU models with a large performance spread; unpaired
# cross-machine numbers mostly measure which VMs the jobs landed on).
#
# Aggregation: per metric, the median of the pairs' percent deltas. A row is
# colored only when |median| exceeds the metric's noise threshold AND at
# least 3/4 of pairs agree in direction. This is a calibrated heuristic, not
# a significance test.
#
# Reports carry an end-to-end-benchmarks.json twin with raw series; when BOTH
# sides of a pair have it, derived estimators (percentiles, sustained-TPS
# slope) are computed here with one implementation for both sides (each side
# runs its own bench binary, so estimators computed in Haskell could silently
# change definition across a comparison). A pair with only one json side
# falls back to comparing the markdown rows, never mixing definitions.
#
#   bench-e2e-diff.py --calibrate DIR       suggest thresholds from A/A runs
#
# where DIR holds one downloaded results tree per A/A run; suggested
# thresholds are the p95 of the null |median pair delta| per metric.

import argparse
import json
import math
import re
import sys
from pathlib import Path
from statistics import StatisticsError, linear_regression, median, quantiles

MS_TO_S = 1e-3

# (row key as emitted by Bench.Summary, display label, direction, display
# scale, closed_loop_only, kind). direction: +1 higher is better, -1 lower,
# 0 neutral context. Open-loop scenarios omit the confirmation-latency rows
# (they restate throughput, see hydra-cluster/README.md) and P99 is omitted
# everywhere (confirmations arrive in per-snapshot bursts, so the top
# percentile is a handful of atoms). kind "count" diffs by absolute delta.
METRICS = [
    ("End-to-end TPS", "End-to-end TPS (tx/s)", +1, 1.0, False, "pct"),
    ("Sustained TPS", "Sustained TPS (tx/s)", +1, 1.0, False, "pct"),
    ("Sustained TPS (slope)", "Sustained TPS, slope (tx/s)", +1, 1.0, False, "pct"),
    ("Backlog drain time (s)", "Backlog drain time (s)", -1, 1.0, False, "pct"),
    ("Snapshots per second", "Snapshots per second (/s)", 0, 1.0, False, "pct"),
    ("Avg txs per snapshot", "Avg txs per snapshot", 0, 1.0, False, "pct"),
    ("Avg. Confirmation Time (ms)", "Avg. Confirmation Time (s)", -1, MS_TO_S, True, "pct"),
    ("P50", "P50 confirmation (s)", -1, MS_TO_S, True, "pct"),
    ("P95", "P95 confirmation (s)", -1, MS_TO_S, True, "pct"),
    ("Tx validation time p50 (ms)", "Tx validation time p50 (s)", -1, MS_TO_S, False, "pct"),
    ("Alloc MB per confirmed tx", "Alloc MB per confirmed tx", -1, 1.0, False, "pct"),
    ("Alloc MB per snapshot", "Alloc MB per snapshot", 0, 1.0, False, "pct"),
    ("Mutator CPU s per 1k txs", "Mutator CPU s per 1k txs", -1, 1.0, False, "pct"),
    ("Max live MB (max node)", "Max live MB (max node)", -1, 1.0, False, "pct"),
    ("Peak node RSS (MB)", "Peak node RSS (MB)", -1, 1.0, False, "pct"),
    ("Number of Invalid txs", "Invalid txs", -1, 1.0, False, "count"),
    ("Incremental commit avg (ms)", "Incremental commit avg (s)", -1, MS_TO_S, False, "pct"),
    ("Incremental decommit avg (ms)", "Incremental decommit avg (s)", -1, MS_TO_S, False, "pct"),
]

# Percent thresholds below which a median delta renders as noise. Recalibrate
# from accumulated A/A (null) runs.
THRESHOLDS = {
    "default": 10.0,
    "Avg. Confirmation Time (ms)": 5.0,
    "P50": 5.0,
    "P95": 5.0,
    # Work counters are nearly machine-independent, so hold them tighter.
    "Alloc MB per confirmed tx": 5.0,
    "Mutator CPU s per 1k txs": 8.0,
}

# Colored regressions beyond this on the headline rates additionally emit a
# ::warning annotation (soft gate, exit code stays 0).
WARN_METRICS = {"End-to-end TPS", "Sustained TPS"}
WARN_PCT = 15.0

REQUIRED_AGREEMENT = 0.75

ROW_RE = re.compile(r"^\|\s*_(?P<key>.+?)_\s*\|\s*(?P<val>.*?)\s*\|")
NUM_RE = re.compile(r"-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?")
REP_DIR_RE = re.compile(r"^rep-(?P<slot>\d+)-(?P<side>branch|master)$")


def parse_value(raw):
    m = NUM_RE.search(raw.replace(",", ""))
    return float(m.group()) if m else None


def percentile(sorted_vals, p):
    # quantiles' n=100 cut points put the p-th percentile at index p-1,
    # linearly interpolated ('inclusive': the data is the whole population).
    if not sorted_vals:
        return None
    if len(sorted_vals) == 1:
        return sorted_vals[0]
    return quantiles(sorted_vals, n=100, method="inclusive")[p - 1]


def sustained_tps_slope(snapshot_series):
    """Least-squares slope of cumulative confirmed txs over time, restricted
    to snapshot points whose cumulative count lies in the middle 80%. Unlike
    endpoint-based trimming it does not move in whole-snapshot steps and works
    from 4 in-window points."""
    pts, cum = [], 0
    for t, n in sorted((float(t), int(n)) for t, n in snapshot_series):
        cum += n
        pts.append((t, cum))
    if cum <= 0:
        return None
    window = [(t, c) for t, c in pts if 0.10 * cum <= c <= 0.90 * cum]
    if len(window) < 4:
        return None
    try:
        return linear_regression(*zip(*window)).slope
    except StatisticsError:  # all window points at one timestamp
        return None


def rts_metrics(node_stats, n_txs, n_snapshots):
    # Mirrors Bench.Summary.rtsAggregates; keep the two in sync.
    if not node_stats or n_txs <= 0 or n_snapshots <= 0:
        return {}
    mb = 1024.0 * 1024.0
    total_alloc_mb = sum(s["allocatedBytes"] for s in node_stats) / mb
    return {
        "Alloc MB per confirmed tx": total_alloc_mb / n_txs,
        "Alloc MB per snapshot": total_alloc_mb / n_snapshots,
        "Mutator CPU s per 1k txs": sum(s["mutatorCpuSeconds"] for s in node_stats) / (n_txs / 1000.0),
        "Max live MB (max node)": max(s["maxLiveBytes"] for s in node_stats) / mb,
    }


def summary_to_record(s):
    """One JSON summary -> the same record shape parse_report yields, with
    estimators recomputed from the raw series."""
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
    slope = sustained_tps_slope(s.get("snapshotSeries") or [])
    if slope is not None:
        metrics["Sustained TPS (slope)"] = slope
    metrics["Backlog drain time (s)"] = s.get("drainSeconds") or 0.0
    n_snapshots = s.get("numberOfSnapshots") or 0
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
        "outcome": outcome,
        "failed": outcome is not None or n_txs == 0,
        "metrics": metrics,
    }


def parse_json_report(path):
    try:
        doc = json.loads(Path(path).read_text(encoding="utf-8"))
    except (OSError, ValueError) as e:
        print(f"WARNING: unreadable JSON report {path}: {e}", file=sys.stderr)
        return {}
    records = {}
    for s in doc.get("summaries", []):
        rec = summary_to_record(s)
        records.setdefault(rec["title"], rec)
    return records


def parse_report(path):
    """{scenario_title: record} plus title order, from `| _key_ | value |`
    rows under `## title` headings. A missing 'Number of txs' row or an
    Outcome row marks the scenario as failed."""
    records, order, rec = {}, [], None
    with open(path, encoding="utf-8") as f:
        for line in f:
            if line.startswith("## "):
                title = line[3:].strip()
                rec = records.setdefault(title, {"title": title, "outcome": None, "metrics": {}})
                if title not in order:
                    order.append(title)
                continue
            if rec is None:
                continue
            m = ROW_RE.match(line)
            if not m:
                continue
            key, raw = m.group("key").strip(), m.group("val").strip()
            if key == "Outcome":
                rec["outcome"] = raw
                continue
            val = parse_value(raw)
            if val is not None:
                rec["metrics"][key] = val
    for rec in records.values():
        rec["failed"] = rec["outcome"] is not None or "Number of txs" not in rec["metrics"]
    return records, order


def collect_results(results_dir):
    """Walk DIR/<machine>/rep-<slot>-<side>/ and merge every report found
    under a rep (open and closed invocations write separately)."""
    machines, scenario_order = {}, []
    root = Path(results_dir)
    for machine_dir in sorted(p for p in root.iterdir() if p.is_dir()):
        machine = {"name": machine_dir.name, "fingerprint": None, "reps": []}
        for fp in sorted(machine_dir.glob("**/fingerprint.json")):
            try:
                machine["fingerprint"] = json.loads(fp.read_text(encoding="utf-8"))
                break
            except (OSError, ValueError):
                pass
        for rep_dir in sorted(p for p in machine_dir.iterdir() if p.is_dir()):
            m = REP_DIR_RE.match(rep_dir.name)
            if not m:
                continue
            scenarios, scenarios_json = {}, {}
            for report in sorted(rep_dir.glob("**/end-to-end-benchmarks.md")):
                recs, order = parse_report(report)
                for title in order:
                    scenarios.setdefault(title, recs[title])
                    if title not in scenario_order:
                        scenario_order.append(title)
                twin = report.with_suffix(".json")
                if twin.exists():
                    for title, rec in parse_json_report(twin).items():
                        scenarios_json.setdefault(title, rec)
            machine["reps"].append(
                {
                    "slot": int(m.group("slot")),
                    "side": m.group("side"),
                    "scenarios": scenarios,
                    "scenarios_json": scenarios_json,
                }
            )
        machine["reps"].sort(key=lambda r: r["slot"])
        if machine["reps"]:
            machines[machine_dir.name] = machine
    return machines, scenario_order


def build_pairs(machines):
    pairs = []
    for machine in machines.values():
        reps = machine["reps"]
        for i in range(0, len(reps) - 1, 2):
            a, b = reps[i], reps[i + 1]
            if {a["side"], b["side"]} != {"branch", "master"}:
                print(f"WARNING: slots {a['slot']},{b['slot']} on {machine['name']} are not a branch/master pair; skipped", file=sys.stderr)
                continue
            old, new = (a, b) if a["side"] == "master" else (b, a)
            pairs.append({"machine": machine["name"], "old": old, "new": new})
    return pairs


def fmt_num(x, decimals=2):
    return f"{x:,.{decimals}f}"


def sign(x):
    return (x > 0) - (x < 0)


def decide_cell(deltas, direction, threshold, kind):
    """(cell text, warn_worthy_regression). deltas are percent for kind
    'pct', absolute for 'count'."""
    n = len(deltas)
    med = median(deltas)
    agreeing = sum(1 for d in deltas if sign(d) == sign(med))
    unit = "%" if kind == "pct" else ""
    body = f"{med:+.1f}{unit}"
    if kind == "count":
        significant = med != 0
    else:
        significant = abs(med) >= threshold and agreeing >= math.ceil(REQUIRED_AGREEMENT * n)
    if not significant or direction == 0:
        return (f"≈ {body}" if not significant else body), False
    improved = (med > 0) == (direction > 0)
    regressed_hard = not improved and kind == "pct" and abs(med) >= WARN_PCT and agreeing == n
    return f"{'🟢' if improved else '🔴'} {body}", regressed_hard


def pair_records(p, title):
    """Prefer the json-derived records (shared estimator definitions) when
    both sides have them; otherwise both sides' markdown rows. Never mix."""
    oj = p["old"].get("scenarios_json", {}).get(title)
    nj = p["new"].get("scenarios_json", {}).get(title)
    if oj and nj:
        return oj, nj
    return p["old"]["scenarios"].get(title), p["new"]["scenarios"].get(title)


def scenario_rows(title, pairs, regressions):
    valid = []
    for p in pairs:
        old, new = pair_records(p, title)
        if old and new and not old["failed"] and not new["failed"]:
            valid.append((old, new))
    if not valid:
        return [], 0
    closed_loop = "closed-loop" in title
    rows = []
    for key, label, direction, scale, closed_only, kind in METRICS:
        if closed_only and not closed_loop:
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
        cell, warn = decide_cell(deltas, direction, THRESHOLDS.get(key, THRESHOLDS["default"]), kind)
        if len(deltas) < len(valid):
            cell += f" ({len(deltas)}/{len(valid)} pairs)"
        if warn and key in WARN_METRICS:
            regressions.append(f"{title}: {label} changed {median(deltas):+.1f}%")
        rows.append(f"| {label} | {fmt_num(median(olds), decimals)} | {fmt_num(median(news), decimals)} | {cell} |")
    return rows, len(valid)


def failed_scenario_lines(title, pairs):
    lines = []
    for side, label in (("new", "PR"), ("old", "master")):
        total, failed, reason = 0, 0, None
        for p in pairs:
            rec = p[side]["scenarios"].get(title)
            if rec is None:
                continue
            total += 1
            if rec["failed"]:
                failed += 1
                reason = reason or rec["outcome"] or "no measurements"
        if total and failed:
            lines.append(f"**FAILED on {label} ({failed}/{total} runs)**: {reason}")
    return lines or ["No results on either side; see the workflow run."]


def same_code_spread(machines, title, key="End-to-end TPS"):
    """Cross-machine spread on identical code per side: the noise an unpaired
    comparison would be exposed to."""
    out = {}
    for side in ("master", "branch"):
        vals = [
            rec["metrics"][key]
            for machine in machines.values()
            for rep in machine["reps"]
            if rep["side"] == side
            for rec in [rep["scenarios"].get(title)]
            if rec and not rec["failed"] and key in rec["metrics"]
        ]
        if len(vals) >= 2 and min(vals) > 0:
            out[side] = 100.0 * (max(vals) - min(vals)) / min(vals)
    return out


def render_details(machines, scenario_order):
    lines = ["", "<details>", "<summary>Per-run raw values</summary>", "",
             "| Machine | Slot | Side | Scenario | E2E TPS | Outcome |", "| -- | -- | -- | -- | -- | -- |"]
    for machine in machines.values():
        for rep in machine["reps"]:
            for title in scenario_order:
                rec = rep["scenarios"].get(title)
                if rec is None:
                    continue
                tps = rec["metrics"].get("End-to-end TPS")
                lines.append(
                    f"| {machine['name']} | {rep['slot']} | {rep['side']} | {title} | "
                    f"{fmt_num(tps) if tps is not None else 'n/a'} | {rec['outcome'] or 'ok'} |"
                )
    return lines + ["", "</details>"]


def render(machines, scenario_order, base_sha=None, head_sha=None, with_footer=True):
    pairs = build_pairs(machines)
    out = ["# End-to-end benchmark differences", ""]
    shas = f"Comparing `{(head_sha or 'PR')[:7]}` (PR) against merge-base `{(base_sha or 'master')[:7]}`. " if (base_sha or head_sha) else ""
    out.append(
        shas
        + "Each runner measures both sides; every delta is the median over the "
        + "same-machine pairs. Colored rows exceed the per-metric noise threshold "
        + "with directional agreement (a calibrated heuristic, not a significance "
        + "test); `≈` is within noise; uncolored rows are context. "
        + "🟢 = improvement, 🔴 = regression."
    )
    out.append("")
    if not pairs:
        out.append("No valid same-machine pairs found; benchmark runs likely failed. See the workflow run for logs.")
        return out, []
    regressions = []
    matched_any = False
    for title in scenario_order:
        rows, n_valid = scenario_rows(title, pairs, regressions)
        if n_valid == 0:
            out += ["", f"## {title}", ""] + failed_scenario_lines(title, pairs)
            matched_any = True
            continue
        header = f"## {title}" + (f" ({n_valid}/{len(pairs)} pairs)" if n_valid < len(pairs) else "")
        out += ["", header, "", "| Metric | master | PR | Δ |", "| -- | -- | -- | -- |"] + rows
        matched_any = True
    if not matched_any:
        out.append("No comparable scenarios found between this PR and `master`.")
    if with_footer:
        out += ["", "---", ""]
        for name, machine in machines.items():
            fp = machine["fingerprint"] or {}
            extra = ", ".join(str(x) for x in (f"{fp.get('nproc')} vCPU" if fp.get("nproc") else None, f"{fp.get('mem_gb')} GB" if fp.get("mem_gb") else None) if x)
            out.append(f"- `{name}`: {fp.get('cpu_model', 'unknown CPU')}{f' ({extra})' if extra else ''}")
        spreads = [
            f"  - {title}: " + ", ".join(f"{side} {pct:.1f}%" for side, pct in sorted(spread.items()))
            for title in scenario_order
            for spread in [same_code_spread(machines, title)]
            if spread
        ]
        if spreads:
            out.append("- Same-code spread across runners (End-to-end TPS), the noise an unpaired comparison would see:")
            out += spreads
        out += render_details(machines, scenario_order)
    return out, regressions


def legacy_machines(old_file, new_file):
    old_recs, _ = parse_report(Path(old_file))
    new_recs, new_order = parse_report(Path(new_file))
    machine = {
        "name": "local",
        "fingerprint": None,
        "reps": [
            {"slot": 1, "side": "master", "scenarios": old_recs, "scenarios_json": {}},
            {"slot": 2, "side": "branch", "scenarios": new_recs, "scenarios_json": {}},
        ],
    }
    order = [t for t in new_order if t in old_recs]
    return {"local": machine}, order


def calibrate(runs_dir):
    """runs_dir holds one downloaded A/A results tree per subdirectory."""
    per_metric = {}
    runs = sorted(p for p in Path(runs_dir).iterdir() if p.is_dir())
    for run in runs:
        machines, scenario_order = collect_results(run)
        pairs = build_pairs(machines)
        for title in scenario_order:
            for key, _, _, scale, _, kind in METRICS:
                if kind != "pct":
                    continue
                deltas = []
                for p in pairs:
                    old, new = pair_records(p, title)
                    if not old or not new or old["failed"] or new["failed"]:
                        continue
                    if key in old["metrics"] and key in new["metrics"] and old["metrics"][key] != 0:
                        deltas.append(100.0 * (new["metrics"][key] - old["metrics"][key]) / old["metrics"][key])
                if deltas:
                    per_metric.setdefault(key, []).append(abs(median(deltas)))
    suggested = {}
    for key, meds in sorted(per_metric.items()):
        meds.sort()
        idx = max(0, min(len(meds) - 1, math.ceil(0.95 * len(meds)) - 1))
        suggested[key] = round(max(meds[idx], 1.0), 1)
    print(json.dumps({"suggested_thresholds": suggested, "runs": len(runs)}, indent=2))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("old_file", nargs="?", help="legacy mode: master end-to-end-benchmarks.md")
    parser.add_argument("new_file", nargs="?", help="legacy mode: PR end-to-end-benchmarks.md")
    parser.add_argument("--results-dir", help="paired mode: directory of per-machine results")
    parser.add_argument("--base-sha", default=None)
    parser.add_argument("--head-sha", default=None)
    parser.add_argument("--calibrate", metavar="DIR",
                        help="suggest thresholds from a directory of A/A results trees")
    args = parser.parse_args()

    if args.calibrate:
        calibrate(args.calibrate)
        return

    if args.results_dir:
        machines, order = collect_results(args.results_dir)
        out, regressions = render(machines, order, args.base_sha, args.head_sha)
    elif args.old_file and args.new_file:
        machines, order = legacy_machines(args.old_file, args.new_file)
        out, regressions = render(machines, order, with_footer=False)
    else:
        parser.error("either --results-dir or two report files are required")
        return

    for regression in regressions:
        print(f"::warning title=Benchmark regression::{regression}", file=sys.stderr)
    print("\n".join(out))


if __name__ == "__main__":
    main()
