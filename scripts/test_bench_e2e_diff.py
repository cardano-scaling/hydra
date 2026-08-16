#! /usr/bin/env python3

# Self-test for bench-e2e-diff.py, run by the comment job before the real
# diff. Stdlib only: python3 scripts/test_bench_e2e_diff.py

import importlib.util
import json
import os
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("bench_e2e_diff", HERE / "bench-e2e-diff.py")
diff = importlib.util.module_from_spec(spec)
spec.loader.exec_module(diff)


def summary(title="Sustained load", load_mode="open-loop", n_txs=1000, tps=500.0,
            wall=2.0, snapshots=None, conf_ms=None, outcome=None, rts=None):
    snapshots = snapshots if snapshots is not None else [[0.5 * i, 100] for i in range(10)]
    return {
        "summaryTitle": title,
        "loadMode": load_mode,
        "numberOfTxs": n_txs,
        "totalTxs": n_txs,
        "numberOfInvalidTxs": 0,
        "numberOfSnapshots": len(snapshots),
        "avgTxsPerSnapshot": n_txs / len(snapshots) if snapshots else 0,
        "endToEndTps": tps,
        "runWallClockSeconds": wall if wall is not None else (n_txs / tps),
        "drainSeconds": 1.0,
        "snapshotSeries": snapshots,
        "confirmationTimesMs": conf_ms or [float(i) for i in range(1, n_txs + 1)],
        "validationP50Ms": 5.0,
        "peakNodeRssMb": 400.0,
        "nodeRtsStats": rts or [],
        "incrementalCommitTimes": [],
        "incrementalDecommitTimes": [],
        "runOutcome": outcome,
    }


def write_rep(root, machine, slot, side, summaries):
    d = root / machine / f"rep-{slot}-{side}" / "open"
    d.mkdir(parents=True, exist_ok=True)
    with open(d / "end-to-end-benchmarks.json", "w", encoding="utf-8") as f:
        json.dump({"version": 1, "summaries": summaries}, f)
    # Twin md so collect_results' md-first walk finds the pair.
    lines = []
    for s in summaries:
        lines += [f"## {s['summaryTitle']}", ""]
        lines += [f"| _Number of txs_ | {s['numberOfTxs']} |"]
    (d / "end-to-end-benchmarks.md").write_text("\n".join(lines), encoding="utf-8")


class SlopeEstimator(unittest.TestCase):
    def test_linear_series_recovers_rate(self):
        # 200 txs every 0.5s = 400 tx/s, 20 points.
        series = [(0.5 * i, 200) for i in range(1, 21)]
        self.assertAlmostEqual(diff.sustained_tps_slope(series), 400.0, places=6)

    def test_ramp_is_trimmed(self):
        # Slow ramp (first 10% of txs), then steady 1000 tx/s: the window
        # excludes the ramp, so the slope reflects the steady phase.
        series = [(10.0, 100)] + [(10.0 + 0.1 * i, 100) for i in range(1, 10)]
        slope = diff.sustained_tps_slope(series)
        self.assertIsNotNone(slope)
        self.assertAlmostEqual(slope, 1000.0, delta=1.0)

    def test_too_few_points(self):
        self.assertIsNone(diff.sustained_tps_slope([(1.0, 100), (2.0, 100), (3.0, 100)]))

    def test_empty(self):
        self.assertIsNone(diff.sustained_tps_slope([]))


class Percentile(unittest.TestCase):
    def test_interpolates(self):
        self.assertEqual(diff.percentile([1.0, 2.0, 3.0, 4.0], 50), 2.5)
        self.assertEqual(diff.percentile([1.0], 95), 1.0)
        self.assertIsNone(diff.percentile([], 50))


class DecideCell(unittest.TestCase):
    def cell(self, deltas, direction=+1, threshold=10.0, kind="pct"):
        return diff.decide_cell(deltas, direction, threshold, kind)

    def test_within_noise(self):
        text, warn, _ = self.cell([2.0, -1.0, 3.0, 1.0])
        self.assertTrue(text.startswith("≈"))
        self.assertFalse(warn)

    def test_pairs_disagree_is_noise(self):
        text, _, _ = self.cell([15.0, -12.0, 14.0, -1.0])
        self.assertTrue(text.startswith("≈"))
        self.assertIn("pairs disagree", text)

    def test_improvement_colored(self):
        text, warn, _ = self.cell([15.0, 12.0, 14.0, 11.0])
        self.assertTrue(text.startswith("🟢"))
        self.assertFalse(warn)

    def test_regression_colored(self):
        text, warn, _ = self.cell([-15.0, -12.0, -14.0, -11.0])
        self.assertTrue(text.startswith("🔴"))
        self.assertFalse(warn)  # not strong: |median| < 2x threshold

    def test_strong_regression_warns(self):
        text, warn, _ = self.cell([-25.0, -22.0, -24.0, -21.0])
        self.assertTrue(text.startswith("🔴"))
        self.assertIn("**", text)
        self.assertTrue(warn)

    def test_three_of_four_agreement_suffices(self):
        text, _, _ = self.cell([15.0, 12.0, 14.0, -1.0])
        self.assertTrue(text.startswith("🟢"))

    def test_lower_is_better(self):
        text, _, _ = self.cell([-15.0, -12.0, -14.0, -11.0], direction=-1)
        self.assertTrue(text.startswith("🟢"))

    def test_neutral_never_colored(self):
        text, _, _ = self.cell([15.0, 12.0, 14.0, 11.0], direction=0)
        self.assertNotIn("🟢", text)
        self.assertNotIn("🔴", text)

    def test_count_kind_uses_absolute_delta(self):
        text, _, _ = self.cell([1.0, 1.0, 2.0, 1.0], direction=-1, kind="count")
        self.assertTrue(text.startswith("🔴"))
        text, _, _ = self.cell([0.0, 0.0, 0.0, 0.0], direction=-1, kind="count")
        self.assertTrue(text.startswith("≈"))

    def test_single_pair_no_spread(self):
        text, _, _ = self.cell([15.0])
        self.assertNotIn("[", text)


class PairingAndRendering(unittest.TestCase):
    def render(self, root):
        machines, order = diff.collect_results(root)
        args = type("A", (), {"base_sha": "a" * 40, "head_sha": "b" * 40})
        thresholds = diff.DEFAULT_THRESHOLDS
        return diff.render_paired(machines, order, args, thresholds)

    def test_aa_run_is_all_noise(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for machine, order in (("m1", ["branch", "master", "master", "branch"]),
                                   ("m2", ["master", "branch", "branch", "master"])):
                for slot, side in enumerate(order, start=1):
                    write_rep(root, machine, slot, side, [summary(tps=500.0 + slot)])
            out, regressions = self.render(root)
            rows = [line for line in out if line.startswith("| ")]
            self.assertFalse(any("🔴" in r or "🟢" in r for r in rows), rows)
            self.assertEqual(regressions, [])

    def test_regression_detected_across_pairs(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for machine, order in (("m1", ["branch", "master", "master", "branch"]),
                                   ("m2", ["master", "branch", "branch", "master"])):
                for slot, side in enumerate(order, start=1):
                    tps = 350.0 if side == "branch" else 500.0
                    write_rep(root, machine, slot, side, [summary(tps=tps, wall=1000.0 / tps)])
            out, regressions = self.render(root)
            text = "\n".join(out)
            self.assertIn("🔴", text)
            self.assertTrue(any("End-to-end TPS" in r for r in regressions))

    def test_missing_rep_drops_only_its_pair(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            write_rep(root, "m1", 1, "branch", [summary()])
            write_rep(root, "m1", 2, "master", [summary()])
            write_rep(root, "m1", 3, "master", [summary()])
            # slot 4 (branch) missing entirely: odd rep count, one pair remains
            machines, _ = diff.collect_results(root)
            pairs = diff.build_pairs(machines)
            self.assertEqual(len(pairs), 1)

    def test_failed_side_rendered_prominently(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            write_rep(root, "m1", 1, "branch", [summary(outcome="confirmed 10 of 1000 txs")])
            write_rep(root, "m1", 2, "master", [summary()])
            out, _ = self.render(root)
            text = "\n".join(out)
            self.assertIn("FAILED on PR", text)
            self.assertIn("confirmed 10 of 1000 txs", text)

    def test_open_loop_hides_latency_rows(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            write_rep(root, "m1", 1, "branch", [summary()])
            write_rep(root, "m1", 2, "master", [summary()])
            out, _ = self.render(root)
            text = "\n".join(out)
            self.assertNotIn("P50 confirmation", text)
            self.assertIn("End-to-end TPS", text)

    def test_closed_loop_shows_latency_rows(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            s = summary(title="Round-trip latency", load_mode="closed-loop")
            write_rep(root, "m1", 1, "branch", [s])
            write_rep(root, "m1", 2, "master", [s])
            out, _ = self.render(root)
            text = "\n".join(out)
            self.assertIn("P50 confirmation", text)
            self.assertNotIn("P99", text)


class MdFallback(unittest.TestCase):
    MD = """
## Sustained load (3 nodes)

| Number of nodes |  3 |
| -- | -- |
| _Number of txs_ | 15000 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 24409.0 |
| _P50_ | 25851.9ms |
| _End-to-end TPS_ | 494.63 tx/s |
| _Sustained TPS_ | 921.37 tx/s |
| _Backlog drain time (s)_ | 29.5 |
| _Snapshots observed_ | 17 |
| _Number of Invalid txs_ | 0 |
"""

    FAILED_MD = """
## Sustained load (3 nodes) (failed)

Benchmark failed at bench/: timeout

| Number of nodes | 3 |
| -- | -- |
| _Outcome_ | did not complete, no measurements |
"""

    def test_parses_rows_and_load_mode(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "r.md"
            p.write_text(self.MD, encoding="utf-8")
            recs, order = diff.parse_md_report(p)
            self.assertEqual(order, ["Sustained load (3 nodes)"])
            rec = recs[order[0]]
            self.assertFalse(rec["failed"])
            self.assertEqual(rec["load_mode"], "open-loop")
            self.assertEqual(rec["metrics"]["End-to-end TPS"], 494.63)

    def test_failed_block_detected(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "r.md"
            p.write_text(self.FAILED_MD, encoding="utf-8")
            recs, order = diff.parse_md_report(p)
            rec = recs[order[0]]
            self.assertTrue(rec["failed"])
            self.assertIn("did not complete", rec["outcome"])

    def test_legacy_mode_smoke(self):
        with tempfile.TemporaryDirectory() as tmp:
            old = Path(tmp) / "old.md"
            new = Path(tmp) / "new.md"
            old.write_text(self.MD, encoding="utf-8")
            new.write_text(self.MD.replace("494.63", "700.00"), encoding="utf-8")
            out, _ = diff.render_legacy(old, new, None, diff.DEFAULT_THRESHOLDS)
            text = "\n".join(out)
            self.assertIn("End-to-end TPS", text)
            self.assertIn("🟢", text)

    def test_format_transition_pair_skipped(self):
        # One side json, the other md-only: the pair must not compare.
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            write_rep(root, "m1", 1, "branch", [summary(title="Sustained load (3 nodes)")])
            d = root / "m1" / "rep-2-master" / "open"
            d.mkdir(parents=True)
            (d / "end-to-end-benchmarks.md").write_text(self.MD, encoding="utf-8")
            machines, order = diff.collect_results(root)
            args = type("A", (), {"base_sha": None, "head_sha": None})
            out, _ = diff.render_paired(machines, order, args, diff.DEFAULT_THRESHOLDS)
            text = "\n".join(out)
            self.assertIn("format differs", text)


class RtsAggregates(unittest.TestCase):
    def test_matches_summary_formula(self):
        stats = [
            {"allocatedBytes": 1024.0 * 1024 * 1000, "mutatorCpuSeconds": 10.0,
             "gcCpuSeconds": 1.0, "maxLiveBytes": 1024.0 * 1024 * 300, "majorGcs": 5.0},
            {"allocatedBytes": 1024.0 * 1024 * 500, "mutatorCpuSeconds": 6.0,
             "gcCpuSeconds": 1.0, "maxLiveBytes": 1024.0 * 1024 * 200, "majorGcs": 4.0},
        ]
        m = diff.rts_metrics(stats, n_txs=1000, n_snapshots=10)
        self.assertAlmostEqual(m["Alloc MB per confirmed tx"], 1.5)
        self.assertAlmostEqual(m["Alloc MB per snapshot"], 150.0)
        self.assertAlmostEqual(m["Mutator CPU s per 1k txs"], 16.0)
        self.assertAlmostEqual(m["Max live MB (max node)"], 300.0)

    def test_empty_stats_yield_nothing(self):
        self.assertEqual(diff.rts_metrics([], 1000, 10), {})


if __name__ == "__main__":
    unittest.main()
