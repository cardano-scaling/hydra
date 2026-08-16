#! /usr/bin/env python3

# Self-test for bench-e2e-diff.py, run by the comment job before the real
# diff: python3 scripts/test_bench_e2e_diff.py

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("bench_e2e_diff", HERE / "bench-e2e-diff.py")
diff = importlib.util.module_from_spec(spec)
spec.loader.exec_module(diff)


def report_md(title="Sustained load", tps=500.0, p50=25000.0, outcome=None):
    lines = [f"## {title}", "", "| Number of nodes |  3 | ", "| -- | -- |"]
    if outcome is not None:
        return "\n".join(lines + [f"| _Outcome_ | {outcome} |"])
    return "\n".join(
        lines
        + [
            "| _Number of txs_ | 15000 |",
            "| _Avg. Confirmation Time (ms)_ | 24409.0 |",
            f"| _P50_ | {p50}ms |",
            "| _P95_ | 29557.8ms |",
            f"| _End-to-end TPS_ | {tps} tx/s |",
            "| _Sustained TPS_ | 921.37 tx/s |",
            "| _Backlog drain time (s)_ | 29.5 |",
            "| _Snapshots observed_ | 17 |",
            "| _Number of Invalid txs_ | 0 |",
        ]
    )


def write_rep(root, machine, slot, side, md):
    d = root / machine / f"rep-{slot}-{side}" / "open"
    d.mkdir(parents=True, exist_ok=True)
    (d / "end-to-end-benchmarks.md").write_text(md, encoding="utf-8")
    return d


def summary_json(title="Sustained load", tps=500.0, n_txs=15000, snapshots=None):
    snapshots = snapshots if snapshots is not None else [[0.5 * i, 1000] for i in range(1, 16)]
    return {
        "summaryTitle": title,
        "loadMode": "open-loop",
        "numberOfTxs": n_txs,
        "numberOfInvalidTxs": 0,
        "numberOfSnapshots": len(snapshots),
        "avgTxsPerSnapshot": n_txs / len(snapshots),
        "endToEndTps": tps,
        "runWallClockSeconds": n_txs / tps,
        "drainSeconds": 5.0,
        "snapshotSeries": snapshots,
        "confirmationTimesMs": [float(i) for i in range(1, 101)],
        "validationP50Ms": 5.0,
        "peakNodeRssMb": 400.0,
        "incrementalCommitTimes": [],
        "incrementalDecommitTimes": [],
        "runOutcome": None,
    }


def write_rep_with_json(root, machine, slot, side, md, summaries):
    d = write_rep(root, machine, slot, side, md)
    (d / "end-to-end-benchmarks.json").write_text(
        json.dumps({"version": 1, "summaries": summaries}), encoding="utf-8"
    )


def render_dir(root):
    machines, order = diff.collect_results(root)
    return diff.render(machines, order, base_sha="a" * 40, head_sha="b" * 40)


class DecideCell(unittest.TestCase):
    def cell(self, deltas, direction=+1, threshold=10.0, kind="pct"):
        return diff.decide_cell(deltas, direction, threshold, kind)

    def test_within_noise(self):
        text, warn = self.cell([2.0, -1.0, 3.0, 1.0])
        self.assertTrue(text.startswith("≈"))
        self.assertFalse(warn)

    def test_disagreeing_pairs_are_noise(self):
        text, _ = self.cell([15.0, -12.0, 14.0, -11.0])
        self.assertTrue(text.startswith("≈"))

    def test_three_of_four_agreement_colors(self):
        text, _ = self.cell([15.0, 12.0, 14.0, -1.0])
        self.assertTrue(text.startswith("🟢"))

    def test_regression_warns_beyond_warn_pct(self):
        text, warn = self.cell([-25.0, -22.0, -24.0, -21.0])
        self.assertTrue(text.startswith("🔴"))
        self.assertTrue(warn)

    def test_mild_regression_does_not_warn(self):
        text, warn = self.cell([-12.0, -11.0, -13.0, -11.0])
        self.assertTrue(text.startswith("🔴"))
        self.assertFalse(warn)

    def test_lower_is_better(self):
        text, _ = self.cell([-15.0, -12.0, -14.0, -11.0], direction=-1)
        self.assertTrue(text.startswith("🟢"))

    def test_neutral_never_colored(self):
        text, _ = self.cell([15.0, 12.0, 14.0, 11.0], direction=0)
        self.assertNotIn("🟢", text)
        self.assertNotIn("🔴", text)

    def test_count_kind_uses_absolute_delta(self):
        text, _ = self.cell([1.0, 1.0, 2.0, 1.0], direction=-1, kind="count")
        self.assertTrue(text.startswith("🔴"))
        text, _ = self.cell([0.0, 0.0, 0.0, 0.0], direction=-1, kind="count")
        self.assertTrue(text.startswith("≈"))

    def test_all_zero_deltas_are_noise(self):
        text, _ = self.cell([0.0, 0.0, 0.0, 0.0])
        self.assertTrue(text.startswith("≈"))


class Estimators(unittest.TestCase):
    def test_slope_recovers_linear_rate(self):
        series = [(0.5 * i, 200) for i in range(1, 21)]
        self.assertAlmostEqual(diff.sustained_tps_slope(series), 400.0, places=6)

    def test_slope_trims_ramp(self):
        # Slow first 10% of txs, then steady 1000 tx/s: the window excludes
        # the ramp point.
        series = [(10.0, 100)] + [(10.0 + 0.1 * i, 100) for i in range(1, 10)]
        self.assertAlmostEqual(diff.sustained_tps_slope(series), 1000.0, delta=1.0)

    def test_slope_needs_four_window_points(self):
        self.assertIsNone(diff.sustained_tps_slope([(1.0, 100), (2.0, 100), (3.0, 100)]))
        self.assertIsNone(diff.sustained_tps_slope([]))

    def test_percentile_interpolates(self):
        self.assertEqual(diff.percentile([1.0, 2.0, 3.0, 4.0], 50), 2.5)
        self.assertIsNone(diff.percentile([], 50))


class JsonPath(unittest.TestCase):
    def test_json_pair_uses_slope_and_python_percentiles(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for slot, side in ((1, "branch"), (2, "master")):
                write_rep_with_json(root, "m1", slot, side, report_md(), [summary_json()])
            out, _ = render_dir(root)
            text = "\n".join(out)
            self.assertIn("Sustained TPS, slope", text)
            rows = [line for line in out if line.startswith("| ")]
            self.assertFalse(any("🔴" in r or "🟢" in r for r in rows), rows)

    def test_mixed_format_pair_falls_back_to_md(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            write_rep_with_json(root, "m1", 1, "branch", report_md(), [summary_json()])
            write_rep(root, "m1", 2, "master", report_md())
            out, _ = render_dir(root)
            text = "\n".join(out)
            self.assertIn("End-to-end TPS", text)
            self.assertIn("Sustained TPS (tx/s)", text)
            self.assertNotIn("slope", text)

    def test_rts_metrics_mirror_haskell_formula(self):
        mb = 1024.0 * 1024.0
        stats = [
            {"allocatedBytes": 1000 * mb, "mutatorCpuSeconds": 10.0, "maxLiveBytes": 300 * mb},
            {"allocatedBytes": 500 * mb, "mutatorCpuSeconds": 6.0, "maxLiveBytes": 200 * mb},
        ]
        m = diff.rts_metrics(stats, n_txs=1000, n_snapshots=10)
        self.assertAlmostEqual(m["Alloc MB per confirmed tx"], 1.5)
        self.assertAlmostEqual(m["Alloc MB per snapshot"], 150.0)
        self.assertAlmostEqual(m["Mutator CPU s per 1k txs"], 16.0)
        self.assertAlmostEqual(m["Max live MB (max node)"], 300.0)
        self.assertEqual(diff.rts_metrics([], 1000, 10), {})

    def test_calibrate_smoke(self):
        import contextlib
        import io

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for run in ("run-1", "run-2"):
                for slot, side in ((1, "branch"), (2, "master")):
                    write_rep(root / run, "m1", slot, side, report_md(tps=500.0 + slot))
            buf = io.StringIO()
            with contextlib.redirect_stdout(buf):
                diff.calibrate(root)
            doc = json.loads(buf.getvalue())
            self.assertEqual(doc["runs"], 2)
            self.assertIn("End-to-end TPS", doc["suggested_thresholds"])


class Parsing(unittest.TestCase):
    def test_rows_parsed_and_failure_detected(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "r.md"
            p.write_text(report_md() + "\n" + report_md(title="Broken", outcome="did not complete, no measurements"), encoding="utf-8")
            recs, order = diff.parse_report(p)
            self.assertEqual(order, ["Sustained load", "Broken"])
            self.assertFalse(recs["Sustained load"]["failed"])
            self.assertEqual(recs["Sustained load"]["metrics"]["End-to-end TPS"], 500.0)
            self.assertTrue(recs["Broken"]["failed"])
            self.assertIn("did not complete", recs["Broken"]["outcome"])


class PairingAndRendering(unittest.TestCase):
    def test_aa_run_is_all_noise(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for machine, order in (("m1", ["branch", "master"]), ("m2", ["master", "branch"])):
                for slot, side in enumerate(order, start=1):
                    write_rep(root, machine, slot, side, report_md(tps=500.0 + slot))
            out, regressions = render_dir(root)
            rows = [line for line in out if line.startswith("| ")]
            self.assertFalse(any("🔴" in r or "🟢" in r for r in rows), rows)
            self.assertEqual(regressions, [])

    def test_regression_detected_and_warned(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for machine, order in (("m1", ["branch", "master"]), ("m2", ["master", "branch"])):
                for slot, side in enumerate(order, start=1):
                    write_rep(root, machine, slot, side, report_md(tps=350.0 if side == "branch" else 500.0))
            out, regressions = render_dir(root)
            self.assertIn("🔴", "\n".join(out))
            self.assertTrue(any("End-to-end TPS" in r for r in regressions))

    def test_odd_rep_drops_only_its_pair(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for slot, side in ((1, "branch"), (2, "master"), (3, "master")):
                write_rep(root, "m1", slot, side, report_md())
            machines, _ = diff.collect_results(root)
            self.assertEqual(len(diff.build_pairs(machines)), 1)

    def test_failed_side_rendered_prominently(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            write_rep(root, "m1", 1, "branch", report_md(outcome="FAILED: confirmed 10 of 15000 txs"))
            write_rep(root, "m1", 2, "master", report_md())
            out, _ = render_dir(root)
            text = "\n".join(out)
            self.assertIn("FAILED on PR", text)
            self.assertIn("confirmed 10 of 15000 txs", text)

    def test_open_loop_hides_latency_rows(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            write_rep(root, "m1", 1, "branch", report_md())
            write_rep(root, "m1", 2, "master", report_md())
            out, _ = render_dir(root)
            text = "\n".join(out)
            self.assertNotIn("P50 confirmation", text)
            self.assertIn("End-to-end TPS", text)

    def test_closed_loop_shows_latency_but_not_p99(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            md = report_md(title="Round-trip latency (closed-loop)")
            write_rep(root, "m1", 1, "branch", md)
            write_rep(root, "m1", 2, "master", md)
            out, _ = render_dir(root)
            text = "\n".join(out)
            self.assertIn("P50 confirmation", text)
            self.assertNotIn("P99", text)

    def test_same_code_spread_in_footer(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for machine, tps in (("m1", 400.0), ("m2", 600.0)):
                write_rep(root, machine, 1, "branch", report_md(tps=tps))
                write_rep(root, machine, 2, "master", report_md(tps=tps))
            (root / "m1" / "fingerprint.json").write_text(json.dumps({"cpu_model": "EPYC 7763", "nproc": 4}), encoding="utf-8")
            out, _ = render_dir(root)
            text = "\n".join(out)
            self.assertIn("Same-code spread", text)
            self.assertIn("EPYC 7763", text)

    def test_legacy_mode_smoke(self):
        with tempfile.TemporaryDirectory() as tmp:
            old = Path(tmp) / "old.md"
            new = Path(tmp) / "new.md"
            old.write_text(report_md(), encoding="utf-8")
            new.write_text(report_md(tps=700.0), encoding="utf-8")
            machines, order = diff.legacy_machines(old, new)
            out, _ = diff.render(machines, order, with_footer=False)
            text = "\n".join(out)
            self.assertIn("End-to-end TPS", text)
            self.assertIn("🟢", text)


if __name__ == "__main__":
    unittest.main()
