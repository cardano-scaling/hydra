#!/usr/bin/env python3
"""Render a captioned film from an editable JSON list of real recording excerpts."""

import argparse
import json
import math
from pathlib import Path
import subprocess

from record import srt_time


def render(edit_path, output):
    edit = json.loads(edit_path.read_text())
    source = (edit_path.parent / edit["source"]).resolve()
    duration = float(
        subprocess.check_output(
            [
                "ffprobe", "-v", "error", "-show_entries", "format=duration",
                "-of", "csv=p=0", str(source),
            ],
            text=True,
        ).strip()
    )
    segments = edit["segments"]
    if not segments:
        raise ValueError("The edit contains no excerpts")
    for segment in segments:
        begin, end = segment["start"], segment["end"]
        speed = segment.get("speed", 1)
        if not math.isfinite(speed) or speed <= 0:
            raise ValueError("Playback speed must be finite and positive")
        if not all(math.isfinite(value) for value in (begin, end)):
            raise ValueError("Excerpt times must be finite")
        if not 0 <= begin < end <= duration:
            raise ValueError(f"Excerpt is outside the recording: {begin}–{end}")
        if not segment["caption"].strip():
            raise ValueError("Every excerpt needs a caption")
    output.mkdir(parents=True, exist_ok=True)
    clips = output / "clips"
    clips.mkdir(exist_ok=True)
    captions = []
    transcript = ["# " + edit["title"], "", "Captions are generated from the editable JSON film script.", ""]
    elapsed = 0
    paths = []
    for index, segment in enumerate(segments, 1):
        source_length = segment["end"] - segment["start"]
        speed = segment.get("speed", 1)
        length = round(source_length / speed * 25) / 25
        path = clips / f"{index:03}.mp4"
        subprocess.run(
            [
                "ffmpeg", "-nostdin", "-hide_banner", "-loglevel", "error", "-y",
                "-ss", str(segment["start"]), "-t", str(source_length), "-i", str(source),
                "-vf", f"setpts=PTS/{speed}", "-t", str(length),
                "-an", "-r", "25", "-c:v", "libx264", "-threads", "2",
                "-preset", "veryfast", "-crf", "18", "-pix_fmt", "yuv420p", str(path),
            ],
            check=True,
        )
        text = segment["caption"].strip()
        interval = f"{srt_time(elapsed)} --> {srt_time(elapsed + length)}"
        captions += [str(index), interval, text, ""]
        transcript += [f"**{interval}**  ", text, ""]
        elapsed += length
        paths.append(path)
    subtitles = output / "captions.srt"
    subtitles.write_text("\n".join(captions))
    (output / "transcript.md").write_text("\n".join(transcript))
    manifest = output / "concat.txt"
    manifest.write_text("".join(f"file '{path.relative_to(output)}'\n" for path in paths))
    clean = output / "uncaptioned.mp4"
    subprocess.run(
        [
            "ffmpeg", "-nostdin", "-hide_banner", "-loglevel", "error", "-y",
            "-f", "concat", "-safe", "0", "-i", str(manifest), "-c", "copy",
            "-movflags", "+faststart", str(clean),
        ],
        check=True,
    )
    subtitle_path = str(subtitles.resolve()).replace("\\", "\\\\").replace(":", "\\:").replace("'", "\\'")
    style = "FontName=DejaVu Sans,FontSize=16,Outline=1,Shadow=0,Alignment=2,MarginV=25,BackColour=&HCC000000,BorderStyle=4"
    subprocess.run(
        [
            "ffmpeg", "-nostdin", "-hide_banner", "-loglevel", "error", "-y",
            "-i", str(clean), "-vf", f"subtitles='{subtitle_path}':force_style='{style}'",
            "-c:v", "libx264", "-threads", "2", "-preset", "veryfast", "-crf", "20",
            "-pix_fmt", "yuv420p", "-movflags", "+faststart", str(output / "metrics-showcase.mp4"),
        ],
        check=True,
    )
    print(f"Rendered {len(segments)} excerpts, {elapsed:.1f} seconds: {output / 'metrics-showcase.mp4'}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--edit", type=Path, required=True)
    parser.add_argument("--output-directory", type=Path, required=True)
    args = parser.parse_args()
    render(args.edit.resolve(), args.output_directory.resolve())
