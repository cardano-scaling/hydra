#!/usr/bin/env python3
"""Record a dashboard while a child scenario writes its live JSONL timeline."""

import argparse
import asyncio
import base64
from contextlib import suppress
from datetime import datetime, timezone
import json
import math
import os
from pathlib import Path
import shutil
import signal
import subprocess
import sys
from urllib.parse import urlparse
from urllib.request import Request, urlopen


def parse_event(line):
    event = json.loads(line)
    if event.get("schemaVersion") != 1:
        raise ValueError("Unsupported timeline schemaVersion")
    for key in ("phase", "status", "message", "timestamp"):
        if not isinstance(event.get(key), str):
            raise ValueError(f"Timeline {key} must be a string")
    elapsed = event.get("elapsedSeconds")
    if (
        isinstance(elapsed, bool)
        or not isinstance(elapsed, (int, float))
        or not math.isfinite(elapsed)
        or elapsed < 0
    ):
        raise ValueError("Timeline elapsedSeconds must be finite and non-negative")
    timestamp = datetime.fromisoformat(event["timestamp"].replace("Z", "+00:00"))
    if timestamp.tzinfo is None:
        raise ValueError("Timeline timestamp must include a timezone")
    return event


class TimelineReader:
    """Keep partial writes until their newline arrives; reject a truncated run."""

    def __init__(self, path):
        self.path = Path(path)
        self.offset = 0
        self.pending = b""

    def read(self, final=False):
        if not self.path.exists():
            return []
        with self.path.open("rb") as stream:
            stream.seek(self.offset)
            self.pending += stream.read()
            self.offset = stream.tell()
        lines = self.pending.split(b"\n")
        self.pending = lines.pop()
        if final and self.pending.strip():
            raise ValueError("Scenario left a truncated timeline event")
        return [parse_event(line) for line in lines if line.strip()]


def offset_seconds(event, started):
    timestamp = datetime.fromisoformat(event["timestamp"].replace("Z", "+00:00"))
    return max(0, (timestamp - started).total_seconds())


def srt_time(seconds):
    milliseconds = round(seconds * 1000)
    hours, milliseconds = divmod(milliseconds, 3_600_000)
    minutes, milliseconds = divmod(milliseconds, 60_000)
    seconds, milliseconds = divmod(milliseconds, 1000)
    return f"{hours:02}:{minutes:02}:{seconds:02},{milliseconds:03}"


def write_subtitles(path, events, started, duration):
    with Path(path).open("w") as stream:
        for index, event in enumerate(events):
            begin = offset_seconds(event, started)
            end = (
                min(duration, offset_seconds(events[index + 1], started))
                if index + 1 < len(events)
                else duration
            )
            if end <= begin:
                continue
            text = " ".join(event["message"].splitlines())
            stream.write(
                f"{index + 1}\n{srt_time(begin)} --> {srt_time(end)}\n{event['phase']}: {text}\n\n"
            )


def annotate(url, token, event, dashboard_uid=None):
    """Annotation failures are errors; a recording must not silently lose them."""
    timestamp = datetime.fromisoformat(event["timestamp"].replace("Z", "+00:00"))
    headers = {"Content-Type": "application/json"}
    if token:
        headers["Authorization"] = f"Bearer {token}"
    elif os.environ.get("GRAFANA_PASSWORD"):
        credentials = f"{os.environ.get('GRAFANA_USER', 'admin')}:{os.environ['GRAFANA_PASSWORD']}"
        headers["Authorization"] = (
            "Basic " + base64.b64encode(credentials.encode()).decode()
        )
    body = {
        "time": round(timestamp.timestamp() * 1000),
        "tags": ["head-lifecycle", event["phase"], event["status"]],
        "text": event["message"],
    }
    if dashboard_uid:
        body["dashboardUID"] = dashboard_uid
    request = Request(
        url,
        data=json.dumps(body).encode(),
        headers=headers,
        method="POST",
    )
    with urlopen(request, timeout=10) as response:
        response.read()


async def stop_child(child):
    if child is None or child.returncode is not None:
        return
    # SIGINT lets the Haskell harness resume a stopped peer and unwind all its
    # process brackets. The process group is only killed if that cleanup hangs.
    child.send_signal(signal.SIGINT)
    try:
        await asyncio.wait_for(child.wait(), timeout=30)
    except asyncio.TimeoutError:
        os.killpg(child.pid, signal.SIGKILL)
        await child.wait()


async def record(args):
    from playwright.async_api import async_playwright

    if args.timeline.exists():
        raise ValueError("Use a fresh scenario directory: the timeline already exists")
    if args.mp4 and shutil.which("ffmpeg") is None:
        raise ValueError("--mp4 requires ffmpeg on PATH")
    args.output_directory.mkdir(parents=True, exist_ok=False)
    events = []
    reader = TimelineReader(args.timeline)
    started = datetime.now(timezone.utc)
    metadata = {
        "startedAt": started.isoformat(),
        "url": args.url,
        "command": args.command,
        "viewport": {"width": 1920, "height": 1080},
    }
    child = None
    status = 1
    video = None
    async with async_playwright() as playwright:
        browser = await playwright.chromium.launch(
            headless=not args.headed,
            executable_path=args.browser_executable,
            # The dashboard uses 2D panels. Avoid GPU/WebGL initialization in
            # containers without a graphics device.
            args=["--disable-gpu", "--disable-webgl", "--disable-software-rasterizer"],
        )
        context = await browser.new_context(
            viewport=metadata["viewport"],
            locale="en-GB",
            record_video_dir=str(args.output_directory),
            record_video_size=metadata["viewport"],
            storage_state=str(args.storage_state) if args.storage_state else None,
        )
        started = datetime.now(timezone.utc)
        metadata["startedAt"] = started.isoformat()
        page = await context.new_page()
        video = page.video
        try:
            await page.goto(args.url, wait_until="domcontentloaded")
            await page.locator(args.ready_selector).first.wait_for(
                state="visible", timeout=60_000
            )
            if urlparse(page.url).path.startswith("/login"):
                raise ValueError(
                    "Grafana requires a login; use --storage-state or a local anonymous viewer"
                )
            if not args.no_overlay:
                await page.evaluate("""() => {
                const caption = document.createElement('div');
                caption.id = 'head-lifecycle-caption';
                Object.assign(caption.style, {position:'fixed', bottom:'12px', left:'12px', right:'12px',
                    zIndex:'2147483647', background:'rgba(0,0,0,0.85)', color:'white', padding:'12px',
                    font:'20px sans-serif', pointerEvents:'none'});
                caption.textContent = 'Waiting for the lifecycle scenario';
                document.body.appendChild(caption);
                }""")

            async def consume(batch):
                for event in batch:
                    events.append(event)
                    if not args.no_overlay:
                        await page.evaluate(
                            "text => { document.getElementById('head-lifecycle-caption').textContent = text; }",
                            f"{event['phase']} · {event['status']}: {event['message']}",
                        )
                    if args.annotation_url:
                        await asyncio.to_thread(
                            annotate,
                            args.annotation_url,
                            os.environ.get("GRAFANA_TOKEN"),
                            event,
                            args.dashboard_uid,
                        )
                    if event["status"] in ("observed", "failed"):
                        await page.screenshot(
                            path=str(
                                args.output_directory
                                / f"{len(events):03}-{event['phase']}.png"
                            )
                        )

            with (args.output_directory / "scenario.log").open("wb") as log:
                child = await asyncio.create_subprocess_exec(
                    *args.command,
                    stdout=log,
                    stderr=asyncio.subprocess.STDOUT,
                    start_new_session=True,
                )
                deadline = asyncio.get_running_loop().time() + args.max_seconds
                while child.returncode is None:
                    await consume(reader.read())
                    if asyncio.get_running_loop().time() > deadline:
                        raise TimeoutError("Recording exceeded --max-seconds")
                    await asyncio.sleep(0.2)
                await consume(reader.read(final=True))
                status = await child.wait()
                if status == 0 and (not events or events[-1]["phase"] != "complete"):
                    raise ValueError(
                        "Scenario exited successfully without a complete timeline event"
                    )
                await asyncio.sleep(args.tail_seconds)
        except BaseException as err:
            metadata["recordingError"] = str(err)
            with suppress(Exception):
                await page.screenshot(path=str(args.output_directory / "failed.png"))
            raise
        finally:
            await stop_child(child)
            metadata["exitCode"] = child.returncode if child else None
            finished = datetime.now(timezone.utc)
            metadata["finishedAt"] = finished.isoformat()
            (args.output_directory / "capture.json").write_text(
                json.dumps(metadata, indent=2) + "\n"
            )
            (args.output_directory / "events.json").write_text(
                json.dumps(events, indent=2) + "\n"
            )
            await context.close()
            if video:
                await video.save_as(str(args.output_directory / "recording.webm"))
            await browser.close()
            duration = (finished - started).total_seconds()
            write_subtitles(
                args.output_directory / "captions.srt", events, started, duration
            )
    if args.mp4:
        subprocess.run(
            [
                "ffmpeg",
                "-nostdin",
                "-i",
                str(args.output_directory / "recording.webm"),
                "-c:v",
                "libx264",
                "-pix_fmt",
                "yuv420p",
                "-movflags",
                "+faststart",
                str(args.output_directory / "recording.mp4"),
            ],
            check=True,
        )
    return status


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--url", required=True, help="Dashboard URL to record")
    parser.add_argument("--timeline", type=Path, required=True)
    parser.add_argument("--output-directory", type=Path, required=True)
    parser.add_argument(
        "--ready-selector",
        default="body",
        help="Visible dashboard element required before launching the scenario",
    )
    parser.add_argument(
        "--storage-state", type=Path, help="Existing Playwright login state"
    )
    parser.add_argument(
        "--browser-executable", default=os.environ.get("CHROMIUM_EXECUTABLE")
    )
    parser.add_argument(
        "--annotation-url",
        help="Optional Grafana /api/annotations URL; token comes from GRAFANA_TOKEN",
    )
    parser.add_argument("--dashboard-uid", help="Scope annotations to this dashboard")
    parser.add_argument("--max-seconds", type=float, default=1200)
    parser.add_argument("--tail-seconds", type=float, default=5)
    parser.add_argument("--headed", action="store_true")
    parser.add_argument(
        "--no-overlay",
        action="store_true",
        help="Leave captions for later editing and rendering",
    )
    parser.add_argument("--mp4", action="store_true")
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    if args.command[:1] == ["--"]:
        args.command = args.command[1:]
    if not args.command:
        parser.error("Provide the scenario command after --")
    if (
        not math.isfinite(args.max_seconds)
        or args.max_seconds <= 0
        or not math.isfinite(args.tail_seconds)
        or args.tail_seconds < 0
    ):
        parser.error(
            "Recording duration must be positive and tail duration non-negative"
        )
    try:
        return asyncio.run(record(args))
    except KeyboardInterrupt:
        return 130
    except Exception as err:
        print(f"Recording failed: {err}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
