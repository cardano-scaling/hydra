#!/usr/bin/env python3
"""Analyze hydra-node JSON logs for per-input and per-snapshot-round timing.

Reads one or more hydra-node log files (one JSON envelope per line, as written
by the tracer) and reports:

  - per-input processing time (EndInput - BeginInput), bucketed by input kind
    (ReqTx, ReqSn, AckSn, other network, chain, client)
  - per-effect dispatch time (EndEffect - BeginEffect), bucketed by effect kind
  - per-snapshot-round wall time: BeginInput of a ReqSn until the LogicOutcome
    whose state changes contain SnapshotConfirmed

Usage:
    bench-logs-analyze.py [--csv PREFIX] LOGFILE...

Only uses the Python standard library, like scripts/bench-e2e-diff.py.
"""

import argparse
import json
import sys
from collections import defaultdict
from datetime import datetime


def parse_ts(ts):
    # Timestamps look like 2026-07-06T12:34:56.789012345Z; Python only handles
    # up to microseconds, so truncate the fractional part.
    ts = ts.rstrip("Z")
    if "." in ts:
        head, frac = ts.split(".", 1)
        ts = head + "." + frac[:6].ljust(6, "0")
        return datetime.fromisoformat(ts)
    return datetime.fromisoformat(ts)


def input_kind(inp):
    tag = inp.get("tag")
    if tag == "NetworkInput":
        ev = inp.get("networkEvent", {})
        if ev.get("tag") == "ReceivedMessage":
            return ev.get("msg", {}).get("tag", "NetworkInput")
        return "Connectivity"
    if tag == "ChainInput":
        return "Chain:" + inp.get("chainEvent", {}).get("tag", "?")
    if tag == "ClientInput":
        return "Client:" + inp.get("clientInput", {}).get("tag", "?")
    return tag or "?"


def effect_kind(eff):
    tag = eff.get("tag", "?")
    if tag == "NetworkEffect":
        # NetworkEffect has a single positional message field
        msg = eff.get("message") or eff.get("contents") or {}
        if isinstance(msg, dict):
            return "Network:" + msg.get("tag", "?")
        return "Network:?"
    return tag


def quantiles(xs):
    xs = sorted(xs)
    n = len(xs)

    def q(p):
        return xs[min(n - 1, int(p * n))]

    return q(0.50), q(0.95), xs[-1]


def fmt_ms(seconds):
    return f"{seconds * 1000:10.2f}"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("logfiles", nargs="+", metavar="LOGFILE")
    parser.add_argument("--csv", metavar="PREFIX", help="also write PREFIX-{inputs,effects,rounds}.csv")
    args = parser.parse_args()

    input_begin = {}  # (by, inputId) -> (ts, kind)
    input_durations = defaultdict(list)  # kind -> [seconds]
    effect_begin = {}  # (by, inputId, effectId) -> (ts, kind)
    effect_durations = defaultdict(list)  # kind -> [seconds]
    reqsn_begin = {}  # by -> ts of the ReqSn currently in flight
    round_durations = defaultdict(list)  # by -> [seconds]
    rows = {"inputs": [], "effects": [], "rounds": []}

    for path in args.logfiles:
        with open(path) as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                try:
                    envelope = json.loads(line)
                except json.JSONDecodeError:
                    continue
                message = envelope.get("message", {})
                if message.get("tag") != "Node":
                    continue
                node = message.get("node", {})
                tag = node.get("tag")
                ts = parse_ts(envelope["timestamp"])
                by = json.dumps(node.get("by", {}), sort_keys=True)

                if tag == "BeginInput":
                    kind = input_kind(node.get("input", {}))
                    input_begin[(by, node["inputId"])] = (ts, kind)
                    if kind == "ReqSn":
                        reqsn_begin[by] = ts
                elif tag == "EndInput":
                    begin = input_begin.pop((by, node["inputId"]), None)
                    if begin:
                        dt = (ts - begin[0]).total_seconds()
                        input_durations[begin[1]].append(dt)
                        rows["inputs"].append((begin[1], dt))
                elif tag == "BeginEffect":
                    kind = effect_kind(node.get("effect", {}))
                    effect_begin[(by, node["inputId"], node["effectId"])] = (ts, kind)
                elif tag == "EndEffect":
                    begin = effect_begin.pop((by, node["inputId"], node["effectId"]), None)
                    if begin:
                        dt = (ts - begin[0]).total_seconds()
                        effect_durations[begin[1]].append(dt)
                        rows["effects"].append((begin[1], dt))
                elif tag == "LogicOutcome":
                    changes = node.get("outcome", {}).get("stateChanges", [])
                    if any(c.get("tag") == "SnapshotConfirmed" for c in changes):
                        begin = reqsn_begin.pop(by, None)
                        if begin:
                            dt = (ts - begin).total_seconds()
                            round_durations[by].append(dt)
                            rows["rounds"].append(("round", dt))

    for title, durations in [
        ("Per-input processing time (ms)", input_durations),
        ("Per-effect dispatch time (ms)", effect_durations),
    ]:
        print(f"\n== {title}")
        print(f"{'kind':<24} {'count':>8} {'p50':>10} {'p95':>10} {'max':>10}")
        for kind in sorted(durations, key=lambda k: -sum(durations[k])):
            xs = durations[kind]
            p50, p95, mx = quantiles(xs)
            print(f"{kind:<24} {len(xs):>8} {fmt_ms(p50)} {fmt_ms(p95)} {fmt_ms(mx)}")

    print("\n== Snapshot round wall time: ReqSn begin -> SnapshotConfirmed (ms)")
    print(f"{'node':<24} {'count':>8} {'p50':>10} {'p95':>10} {'max':>10}")
    all_rounds = []
    for by in sorted(round_durations):
        xs = round_durations[by]
        all_rounds.extend(xs)
        p50, p95, mx = quantiles(xs)
        label = by[:22]
        print(f"{label:<24} {len(xs):>8} {fmt_ms(p50)} {fmt_ms(p95)} {fmt_ms(mx)}")
    if all_rounds:
        p50, p95, mx = quantiles(all_rounds)
        print(f"{'ALL':<24} {len(all_rounds):>8} {fmt_ms(p50)} {fmt_ms(p95)} {fmt_ms(mx)}")
        print(f"\nImplied max snapshot rate at p50: {1 / p50:.2f} snapshots/s")
    else:
        print("(no completed snapshot rounds found)")

    if args.csv:
        for name, data in rows.items():
            path = f"{args.csv}-{name}.csv"
            with open(path, "w") as f:
                f.write("kind,seconds\n")
                for kind, dt in data:
                    f.write(f"{kind},{dt}\n")
            print(f"wrote {path}", file=sys.stderr)


if __name__ == "__main__":
    main()
