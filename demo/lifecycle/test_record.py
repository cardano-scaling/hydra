import importlib.util
import json
import os
import socket
from datetime import datetime, timezone
from pathlib import Path
import tempfile
import subprocess
import sys
import unittest

spec = importlib.util.spec_from_file_location(
    "record", Path(__file__).with_name("record.py")
)
record = importlib.util.module_from_spec(spec)
spec.loader.exec_module(record)
spec = importlib.util.spec_from_file_location(
    "lifecycle_run", Path(__file__).with_name("run.py")
)
launcher = importlib.util.module_from_spec(spec)
spec.loader.exec_module(launcher)


class LauncherTests(unittest.TestCase):
    def test_refuses_a_port_owned_by_another_server(self):
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            listener.listen()
            with self.assertRaisesRegex(RuntimeError, "occupied"):
                launcher.check_ports([listener.getsockname()[1]])


class TimelineTests(unittest.TestCase):
    def event(self):
        return dict(
            schemaVersion=1,
            timestamp="2026-10-28T12:00:00Z",
            elapsedSeconds=0,
            phase="ready",
            status="observed",
            message="Participants connected",
        )

    def test_partial_writes_are_delivered_once(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "timeline.jsonl"
            reader = record.TimelineReader(path)
            self.assertEqual(reader.read(), [])
            data = json.dumps(self.event()).encode()
            path.write_bytes(data[:20])
            self.assertEqual(reader.read(), [])
            with path.open("ab") as stream:
                stream.write(data[20:] + b"\n")
            self.assertEqual(reader.read(), [self.event()])
            self.assertEqual(reader.read(final=True), [])

    def test_incomplete_final_event_is_an_error(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "timeline.jsonl"
            path.write_text('{"schemaVersion":')
            with self.assertRaisesRegex(ValueError, "truncated"):
                record.TimelineReader(path).read(final=True)

    def test_rejects_unknown_schema_and_invalid_timing(self):
        for key, value in [
            ("schemaVersion", 2),
            ("elapsedSeconds", -1),
            ("elapsedSeconds", float("nan")),
            ("timestamp", "2026-10-28T12:00:00"),
        ]:
            event = self.event()
            event[key] = value
            with self.subTest(key=key, value=value), self.assertRaises(ValueError):
                record.parse_event(json.dumps(event))

    def test_subtitles_use_capture_time_not_scenario_elapsed_time(self):
        started = datetime(2026, 10, 28, 11, 59, 55, tzinfo=timezone.utc)
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "captions.srt"
            record.write_subtitles(path, [self.event()], started, 12)
            self.assertIn("00:00:05,000 --> 00:00:12,000", path.read_text())


@unittest.skipUnless(
    os.environ.get("HYDRA_RECORD_BROWSER_TESTS") == "1",
    "Set HYDRA_RECORD_BROWSER_TESTS=1 in the dashboard-recording shell",
)
class BrowserRecordingTests(unittest.TestCase):
    def test_records_video_and_preserves_a_failed_scenario(self):
        for exit_code in [0, 7]:
            with (
                self.subTest(exit_code=exit_code),
                tempfile.TemporaryDirectory() as directory,
            ):
                root = Path(directory)
                html = root / "fixture.html"
                html.write_text(
                    "<h1>Recording mechanics fixture</h1><canvas width='800' height='600'></canvas>"
                )
                timeline = root / "timeline.jsonl"
                script = root / "scenario.py"
                script.write_text("""import datetime, json, sys, time
from pathlib import Path
with Path(sys.argv[1]).open('w', buffering=1) as f:
    for phase, elapsed in [('ready', 0), ('complete' if int(sys.argv[2]) == 0 else 'failed', 2)]:
        f.write(json.dumps(dict(schemaVersion=1, timestamp=datetime.datetime.now(datetime.timezone.utc).isoformat(), elapsedSeconds=elapsed, phase=phase, status='observed', message=phase)) + '\\n')
        time.sleep(2)
sys.exit(int(sys.argv[2]))
""")
                output = root / "video"
                result = subprocess.run(
                    [
                        sys.executable,
                        str(Path(__file__).with_name("record.py")),
                        "--url",
                        html.as_uri(),
                        "--ready-selector",
                        "h1",
                        "--timeline",
                        str(timeline),
                        "--output-directory",
                        str(output),
                        "--tail-seconds",
                        "0",
                        "--mp4",
                        "--",
                        sys.executable,
                        str(script),
                        str(timeline),
                        str(exit_code),
                    ],
                    capture_output=True,
                    text=True,
                    timeout=60,
                )
                self.assertEqual(result.returncode, exit_code, result.stderr)
                self.assertTrue((output / "recording.webm").stat().st_size > 1000)
                self.assertTrue((output / "recording.mp4").stat().st_size > 1000)
                self.assertEqual(
                    json.loads((output / "capture.json").read_text())["exitCode"],
                    exit_code,
                )
                probe = subprocess.check_output(
                    [
                        "ffprobe",
                        "-v",
                        "quiet",
                        "-of",
                        "json",
                        "-show_streams",
                        str(output / "recording.mp4"),
                    ],
                    text=True,
                )
                stream = json.loads(probe)["streams"][0]
                self.assertEqual((stream["width"], stream["height"]), (1920, 1080))


if __name__ == "__main__":
    unittest.main()
