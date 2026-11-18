"""Regression tests for the demo network configuration."""

import datetime
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


class PrepareDevnetTests(unittest.TestCase):
    def test_genesis_times_agree_when_clock_crosses_a_second(self):
        repo = Path(__file__).resolve().parent.parent
        with tempfile.TemporaryDirectory(prefix="hydra-devnet-") as directory:
            work = Path(directory)
            clock = work / "date"
            # Each clock read advances one second, making independent reads of
            # the two genesis timestamps disagree without a timing dependency.
            clock.write_text(
                "#!/usr/bin/env python3\n"
                "import datetime, os, pathlib, sys\n"
                "state = pathlib.Path(os.environ['TEST_CLOCK_STATE'])\n"
                "seconds = int(state.read_text()) if state.exists() else 1700000000\n"
                "state.write_text(str(seconds + 1))\n"
                "fmt = sys.argv[-1].removeprefix('+').replace('%s', str(seconds))\n"
                "print(datetime.datetime.fromtimestamp(seconds, datetime.timezone.utc).strftime(fmt))\n"
            )
            clock.chmod(0o755)
            target = work / "devnet"
            subprocess.run(
                ["bash", str(repo / "demo/prepare-devnet.sh")],
                env={
                    **os.environ,
                    "BASEDIR": str(repo),
                    "TARGETDIR": str(target),
                    "PATH": f"{work}{os.pathsep}{os.environ['PATH']}",
                    "TEST_CLOCK_STATE": str(work / "clock-state"),
                },
                check=True,
                capture_output=True,
                text=True,
            )
            byron = json.loads((target / "genesis-byron.json").read_text())
            shelley = json.loads((target / "genesis-shelley.json").read_text())
            shelley_start = datetime.datetime.fromisoformat(
                shelley["systemStart"].replace("Z", "+00:00")
            ).timestamp()
            self.assertEqual(byron["startTime"], shelley_start)


if __name__ == "__main__":
    unittest.main()
