#!/usr/bin/env python3
"""Start isolated local monitoring and record the benchmark lifecycle scenario."""

import argparse
from contextlib import ExitStack, contextmanager
import json
import os
from pathlib import Path
import secrets
import signal
import socket
import subprocess
import sys
import time
from urllib.error import URLError
from urllib.request import urlopen


@contextmanager
def service(command, log):
    with log.open("wb") as stream:
        child = subprocess.Popen(
            command, stdout=stream, stderr=subprocess.STDOUT, start_new_session=True
        )
        try:
            yield child
        finally:
            if child.poll() is None:
                child.terminate()
                try:
                    child.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    os.killpg(child.pid, signal.SIGKILL)
                    child.wait()


def wait_ready(child, url):
    deadline = time.monotonic() + 60
    while time.monotonic() < deadline:
        if child.poll() is not None:
            raise RuntimeError(f"Service exited before becoming ready: {url}")
        try:
            with urlopen(url, timeout=1) as response:
                if response.status == 200:
                    return
        except (URLError, TimeoutError):
            pass
        time.sleep(0.2)
    raise TimeoutError(f"Service did not become ready: {url}")


def check_ports(ports):
    if len(set(ports)) != len(ports) or any(not 1 <= port <= 65535 for port in ports):
        raise ValueError("Monitoring ports must be distinct and between 1 and 65535")
    for port in ports:
        with socket.socket() as listener:
            listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            try:
                listener.bind(("127.0.0.1", port))
            except OSError as err:
                raise RuntimeError(
                    f"Monitoring port {port} is already occupied"
                ) from err


def prepare(root, prometheus_port, grafana_port, dashboard_path=None):
    root.mkdir(parents=True, exist_ok=False)
    repo = Path(__file__).resolve().parents[2]
    provisioning = root / "provisioning"
    for name in ["datasources", "dashboards"]:
        (provisioning / name).mkdir(parents=True)
    dashboard = json.loads(
        (
            dashboard_path or repo / "demo/grafana/nix/dashboards/dashboard.json"
        ).read_text()
    )

    def normalize(value):
        if isinstance(value, dict):
            if value.get("type") == "prometheus" and "uid" in value:
                value["uid"] = "prometheus"
            for item in value.values():
                normalize(item)
        elif isinstance(value, list):
            for item in value:
                normalize(item)

    normalize(dashboard)
    for variable in dashboard.get("templating", {}).get("list", []):
        if variable.get("name") == "instance":
            variable["regex"] = ""
            variable["current"] = {"text": "All", "value": "$__all"}
    dashboard["id"] = None
    dashboard["refresh"] = "5s"
    dashboards = root / "dashboards"
    dashboards.mkdir()
    (dashboards / "hydra.json").write_text(json.dumps(dashboard))
    # JSON is valid YAML and avoids another configuration-file dependency.
    (root / "prometheus.yml").write_text(
        json.dumps(
            {
                "global": {
                    "scrape_interval": "2s",
                    "scrape_timeout": "1s",
                    "evaluation_interval": "2s",
                },
                "scrape_configs": [
                    {
                        "job_name": "hydra_nodes",
                        "file_sd_configs": [
                            {
                                "files": [str(root / "scenario/targets.json")],
                                "refresh_interval": "1s",
                            }
                        ],
                    }
                ],
            },
            indent=2,
        )
    )
    (provisioning / "datasources/default.yml").write_text(
        json.dumps(
            {
                "apiVersion": 1,
                "datasources": [
                    {
                        "name": "prometheus",
                        "uid": "prometheus",
                        "type": "prometheus",
                        "access": "proxy",
                        "url": f"http://127.0.0.1:{prometheus_port}",
                        "isDefault": True,
                        "jsonData": {"timeInterval": "2s"},
                    }
                ],
            }
        )
    )
    (provisioning / "dashboards/default.yml").write_text(
        json.dumps(
            {
                "apiVersion": 1,
                "providers": [
                    {
                        "name": "lifecycle",
                        "type": "file",
                        "options": {"path": str(dashboards)},
                    }
                ],
            }
        )
    )
    password = secrets.token_hex(24)
    config = root / "grafana.ini"
    config.write_text(f"""[server]
http_addr = 127.0.0.1
http_port = {grafana_port}
[paths]
data = {root / "grafana-data"}
logs = {root / "grafana-logs"}
plugins = {root / "grafana-plugins"}
provisioning = {provisioning}
[auth.anonymous]
enabled = true
org_role = Viewer
[security]
admin_user = admin
admin_password = {password}
[analytics]
reporting_enabled = false
check_for_updates = false
""")
    config.chmod(0o600)
    return dashboard["uid"], password


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output-directory",
        type=Path,
        required=True,
        help="Fresh directory for monitoring state, scenario and video",
    )
    parser.add_argument("--prometheus-port", type=int, default=9090)
    parser.add_argument("--grafana-port", type=int, default=3000)
    parser.add_argument("--number-of-txs", type=int, default=12)
    parser.add_argument("--seed", type=int, default=42)
    parser.add_argument("--phase-seconds", type=int, default=10)
    parser.add_argument("--peer-pause-seconds", type=int, default=20)
    parser.add_argument("--tx-interval-ms", type=int, default=250)
    parser.add_argument("--mp4", action="store_true")
    parser.add_argument("--headed", action="store_true")
    parser.add_argument("--dashboard", type=Path, help="Dashboard JSON to provision")
    parser.add_argument(
        "--no-overlay",
        action="store_true",
        help="Leave captions for later editing and rendering",
    )
    args = parser.parse_args()
    root = args.output_directory.resolve()
    grafana_home = os.environ.get("GRAFANA_HOME")
    if not grafana_home:
        parser.error("GRAFANA_HOME is required; use nix develop .#head-lifecycle-demo")
    check_ports([args.prometheus_port, args.grafana_port])
    uid, password = prepare(
        root, args.prometheus_port, args.grafana_port, args.dashboard
    )
    base_url = f"http://127.0.0.1:{args.grafana_port}"
    env = dict(os.environ, GRAFANA_USER="admin", GRAFANA_PASSWORD=password)
    here = Path(__file__).resolve().parent
    command = [
        sys.executable,
        str(here / "record.py"),
        "--url",
        f"{base_url}/d/{uid}/hydra?orgId=1&kiosk",
        "--ready-selector",
        '[data-testid="data-testid panel content"]',
        "--dashboard-uid",
        uid,
        "--annotation-url",
        f"{base_url}/api/annotations",
        "--timeline",
        str(root / "scenario/timeline.jsonl"),
        "--output-directory",
        str(root / "video"),
    ]
    command += [
        flag
        for flag, enabled in [
            ("--mp4", args.mp4),
            ("--headed", args.headed),
            ("--no-overlay", args.no_overlay),
        ]
        if enabled
    ]
    command += [
        "--",
        "bench-e2e",
        "lifecycle",
        "--output-directory",
        str(root / "scenario"),
        "--seed",
        str(args.seed),
        "--number-of-txs",
        str(args.number_of_txs),
        "--phase-seconds",
        str(args.phase_seconds),
        "--peer-pause-seconds",
        str(args.peer_pause_seconds),
        "--tx-interval-ms",
        str(args.tx_interval_ms),
    ]
    with ExitStack() as stack:
        prometheus = stack.enter_context(
            service(
                [
                    "prometheus",
                    f"--config.file={root / 'prometheus.yml'}",
                    f"--storage.tsdb.path={root / 'prometheus-data'}",
                    f"--web.listen-address=127.0.0.1:{args.prometheus_port}",
                ],
                root / "prometheus.log",
            )
        )
        grafana = stack.enter_context(
            service(
                [
                    "grafana",
                    "server",
                    "--homepath",
                    grafana_home,
                    "--config",
                    str(root / "grafana.ini"),
                ],
                root / "grafana.log",
            )
        )
        wait_ready(prometheus, f"http://127.0.0.1:{args.prometheus_port}/-/ready")
        wait_ready(grafana, base_url + "/api/health")
        return subprocess.run(command, env=env, check=False).returncode


if __name__ == "__main__":
    try:
        sys.exit(main())
    except KeyboardInterrupt:
        sys.exit(130)
    except Exception as err:
        print(f"Lifecycle demo failed: {err}", file=sys.stderr)
        sys.exit(1)
