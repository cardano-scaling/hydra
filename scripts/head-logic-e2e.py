#!/usr/bin/env python3
"""Run the same local cluster workload with two explicitly supplied node binaries."""

import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import platform
import subprocess


def sha256(path):
    with path.open("rb") as source:
        return hashlib.file_digest(source, "sha256").hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline", type=Path, required=True)
    parser.add_argument("--program", type=Path, required=True)
    parser.add_argument("--bench", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--transactions", type=int, default=10000, help="per client")
    parser.add_argument("--pairs", type=int, default=2, help="alternate AB and BA")
    parser.add_argument(
        "--wait-for-confirmation", action="store_true",
        help="one outstanding transaction per client",
    )
    parser.add_argument("--shapes", nargs="+", default=["Constant", "Plateau 1000"])
    args = parser.parse_args()
    if args.pairs < 2 or args.pairs % 2 or args.transactions < 1:
        parser.error("use a positive even number of pairs and a positive transaction count")
    root = Path(__file__).resolve().parent.parent
    args.output = args.output.resolve()
    args.output.mkdir(parents=True, exist_ok=False)
    binaries = {side: getattr(args, side).resolve(strict=True) for side in ("baseline", "program")}
    bench = args.bench.resolve(strict=True)
    env = {
        key: value for key, value in os.environ.items()
        if key.lower() not in ("http_proxy", "https_proxy", "all_proxy")
    }
    env.update(
        hydra_cluster_datadir=str(root / "hydra-cluster"),
        hydra_node_datadir=str(root / "hydra-node"),
        HYDRA_NODE_RTS_FLAGS="-N1 -T",
    )
    # PATH selection affects only the child processes; no production mode switch.
    for side, binary in binaries.items():
        directory = args.output / "bin" / side
        directory.mkdir(parents=True)
        (directory / "hydra-node").symlink_to(binary)
    report = {
        "schemaVersion": 1,
        "machine": platform.platform(),
        "cpuModel": next(
            (
                line.split(":", 1)[1].strip()
                for line in Path("/proc/cpuinfo").read_text().splitlines()
                if line.startswith("model name")
            ),
            platform.processor(),
        ),
        "cpuAffinity": sorted(os.sched_getaffinity(0)),
        "nodeRtsFlags": env["HYDRA_NODE_RTS_FLAGS"],
        "binaries": {side: {"path": str(binary), "sha256": sha256(binary)} for side, binary in binaries.items()},
        "benchSha256": sha256(bench),
        "runs": [],
    }
    dataset_hashes = {}
    for shape_index, shape in enumerate(args.shapes):
        for pair in range(args.pairs):
            for side in (("baseline", "program") if pair % 2 == 0 else ("program", "baseline")):
                directory = args.output / f"shape-{shape_index}-pair-{pair}-{side}"
                command = [
                    str(bench), "datasets", "--cluster-size", "3",
                    "--number-of-txs", str(args.transactions),
                    "--utxo-size", shape, "--seed", "42", "--timeout", "600s",
                    "--output-directory", str(directory),
                ]
                if args.wait_for_confirmation:
                    command.append("--wait-for-tx-valid")
                binary_dir = str((args.output / "bin" / side).resolve())
                run_env = env | {"PATH": binary_dir + os.pathsep + env["PATH"]}
                print(f"{shape}, pair {pair + 1}, {side}", flush=True)
                started = datetime.now(timezone.utc).isoformat()
                with directory.with_suffix(".log").open("w") as log:
                    completed = subprocess.run(
                        command, cwd=root, env=run_env, stdout=log,
                        stderr=subprocess.STDOUT, check=False,
                    )
                entry = {
                    "shape": shape, "pair": pair, "side": side,
                    "command": command, "exitCode": completed.returncode,
                    "startedAt": started,
                    "finishedAt": datetime.now(timezone.utc).isoformat(),
                }
                dataset = directory / "dataset.json"
                if dataset.exists():
                    entry["datasetSha256"] = sha256(dataset)
                    expected = dataset_hashes.setdefault(shape, entry["datasetSha256"])
                    if expected != entry["datasetSha256"]:
                        raise RuntimeError("datasets differ within the comparison")
                result = directory / "end-to-end-benchmarks.json"
                if result.exists():
                    entry["result"] = json.loads(result.read_text())
                report["runs"].append(entry)
                (args.output / "comparison.json").write_text(json.dumps(report, indent=2) + "\n")
                if completed.returncode:
                    raise SystemExit(f"benchmark failed; see {directory.with_suffix('.log')}")


if __name__ == "__main__":
    main()
