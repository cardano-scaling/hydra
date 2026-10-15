#!/usr/bin/env python3
"""Exercise a release binary, embedded etcd, and persisted state on its host OS."""

import argparse
import ctypes
import json
import os
from pathlib import Path
import signal
import socket
import struct
import subprocess
import time
from urllib.request import urlopen


def greeting(port: int):
    # Only receive the initial, unfragmented server greeting. No client frames
    # or optional WebSocket extensions are needed for this smoke test.
    with socket.create_connection(("127.0.0.1", port), timeout=3) as connection:
        connection.sendall((
            f"GET / HTTP/1.1\r\nHost: 127.0.0.1:{port}\r\n"
            "Upgrade: websocket\r\nConnection: Upgrade\r\n"
            "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
            "Sec-WebSocket-Version: 13\r\n\r\n"
        ).encode("ascii"))
        with connection.makefile("rb") as stream:
            assert b"101" in stream.readline(), "WebSocket upgrade failed"
            while True:
                header = stream.readline()
                assert header, "WebSocket closed during upgrade"
                if header == b"\r\n":
                    break
            opcode, size = stream.read(2)
            assert opcode == 0x81 and not size & 0x80, "Expected a text greeting"
            if size == 126:
                size = struct.unpack("!H", stream.read(2))[0]
            elif size == 127:
                size = struct.unpack("!Q", stream.read(8))[0]
            result = json.loads(stream.read(size))
            assert result["tag"] == "Greetings"
            return result


def eventually(action, processes, timeout=90):
    deadline = time.monotonic() + timeout
    last_error = None
    while time.monotonic() < deadline:
        for process in processes:
            assert process.poll() is None, f"Node exited early: {process.returncode}"
        try:
            return action()
        except (OSError, AssertionError, ValueError) as error:
            last_error = error
            time.sleep(0.25)
    raise AssertionError(f"Timed out: {last_error}")


def check_node(index: int):
    with urlopen(f"http://127.0.0.1:{4101 + index}/config", timeout=3) as response:
        assert json.load(response)
    status = greeting(4101 + index)
    assert status.get("hydraHeadId") is not None
    assert status["networkInfo"]["networkConnected"]
    peers = status["networkInfo"]["peersInfo"]
    assert peers and all(peers.values()), f"Peer not connected: {peers}"
    return status["hydraHeadId"]


def port_is_closed(port: int):
    with socket.socket() as connection:
        connection.settimeout(1)
        assert connection.connect_ex(("127.0.0.1", port)) != 0, f"Port {port} still open"


def check_shutdown(process, log: Path):
    code = process.wait(timeout=30)
    lines = log.read_text(encoding="utf-8").splitlines()
    # Main catches UserInterrupt with the other exceptions and calls die.
    # Exit code 1 alone would also accept an unexpected shutdown exception.
    assert code == 1 and lines and lines[-1] == "user interrupt", (
        f"Unexpected shutdown: exit {code}, log {log}\n" + "\n".join(lines[-5:])
    )


def smoke_test(binary: Path, work: Path, parameters: Path, catalogue: Path):
    if os.name == "nt":
        # CTRL_BREAK_EVENT requires a shared console. CI may launch Python
        # without one; children inherit the console allocated here.
        kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
        console_process = ctypes.c_ulong()
        attached = kernel32.GetConsoleProcessList(ctypes.byref(console_process), 1)
        if not attached and not kernel32.AllocConsole():
            raise ctypes.WinError(ctypes.get_last_error())
    work.mkdir(parents=True)
    version = subprocess.check_output([binary, "--version"], text=True, timeout=30)
    print(version.strip(), flush=True)
    subprocess.run([binary, "--help"], check=True, stdout=subprocess.DEVNULL, timeout=30)
    actual = json.loads(subprocess.check_output([binary, "--hydra-script-catalogue"], timeout=30))
    assert actual == json.loads(catalogue.read_text()), "Scripts differ from the native Linux build"
    initial_utxo = work / "initial utxo.json"
    initial_utxo.write_text("{}")
    for name in ("alice", "bob"):
        subprocess.run([binary, "gen-hydra-key", "--output-file", work / name], check=True, timeout=30)
        assert json.loads((work / f"{name}.sk").read_text())["cborHex"]
        assert json.loads((work / f"{name}.vk").read_text())["cborHex"]

    head_ids = None
    for run in range(2):
        processes, logs = [], []
        try:
            for index, (name, peer) in enumerate((("alice", "bob"), ("bob", "alice"))):
                state = work / f"{name} state"
                command = [
                    str(binary), "--node-id", name,
                    "--listen", f"127.0.0.1:{5101 + index}",
                    "--peer", f"127.0.0.1:{5102 - index}",
                    "--api-host", "127.0.0.1", "--api-port", str(4101 + index),
                    "--hydra-signing-key", str(work / f"{name}.sk"),
                    "--hydra-verification-key", str(work / f"{peer}.vk"),
                    "--persistence-dir", str(state),
                    "--offline-head-seed", "00" * 32,
                    "--initial-utxo", str(initial_utxo),
                    "--ledger-protocol-parameters", str(parameters),
                ]
                log = (work / f"{name}-{run}.log").open("wb")
                logs.append(log)
                options = ({"creationflags": subprocess.CREATE_NEW_PROCESS_GROUP}
                           if os.name == "nt" else {"start_new_session": True})
                processes.append(subprocess.Popen(command, stdout=log, stderr=subprocess.STDOUT, **options))

            current_ids = [eventually(lambda i=i: check_node(i), processes) for i in range(2)]
            assert current_ids[0] == current_ids[1]
            if head_ids is not None:
                assert current_ids == head_ids, "Head identity changed after restart"
            head_ids = current_ids
            for name in ("alice", "bob"):
                state = work / f"{name} state"
                assert (state / "hydra.db").stat().st_size > 0
                assert (state / "bin" / ("etcd.exe" if os.name == "nt" else "etcd")).is_file()
            print(f"Startup, API and peers passed (run {run + 1}/2).", flush=True)
            for process in processes:
                process.send_signal(signal.CTRL_BREAK_EVENT if os.name == "nt" else signal.SIGINT)
            for process, log in zip(processes, logs):
                check_shutdown(process, Path(log.name))
            for port in (2479, 2480, 4101, 4102, 5101, 5102):
                eventually(lambda p=port: port_is_closed(p), [], timeout=15)
        finally:
            # Give the node a chance to stop etcd even when an assertion fails.
            for process in processes:
                if process.poll() is None:
                    process.send_signal(signal.CTRL_BREAK_EVENT if os.name == "nt" else signal.SIGINT)
            for process in processes:
                if process.poll() is None:
                    try:
                        process.wait(timeout=30)
                    except subprocess.TimeoutExpired:
                        if os.name == "nt":
                            subprocess.run(["taskkill", "/PID", str(process.pid), "/T", "/F"], check=False)
                        else:
                            os.killpg(process.pid, signal.SIGKILL)
                        process.wait(timeout=10)
            for log in logs:
                log.close()
        if run == 1:
            for name in ("alice", "bob"):
                events = (json.loads(line)["message"].get("node", {})
                          for line in (work / f"{name}-{run}.log").read_text(encoding="utf-8").splitlines()
                          if line.startswith("{"))
                assert any(event.get("tag") == "LoadedState" and event.get("lastEventId") is not None
                           for event in events), f"{name} did not reload persisted events"
    print("CLI, scripts, keys, API, peers, embedded etcd, shutdown and restart passed.", flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("binary", type=Path)
    parser.add_argument("catalogue", type=Path, help="Catalogue generated by the native Linux build")
    parser.add_argument("--work-dir", type=Path, default=Path("binary smoke test"))
    parser.add_argument("--parameters", type=Path, default=Path("hydra-cluster/config/protocol-parameters.json"))
    args = parser.parse_args()
    smoke_test(args.binary.resolve(), args.work_dir.resolve(), args.parameters.resolve(), args.catalogue.resolve())
