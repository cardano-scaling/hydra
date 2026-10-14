#!/usr/bin/env python3
"""Collect a Windows release and reject missing non-system DLLs."""

from pathlib import Path
import re
import shutil
import subprocess
import sys


SYSTEM_DLLS = {
    "advapi32.dll", "bcrypt.dll", "bcryptprimitives.dll", "comctl32.dll", "comdlg32.dll",
    "crypt32.dll", "dbghelp.dll", "dnsapi.dll", "gdi32.dll", "imm32.dll",
    "iphlpapi.dll", "kernel32.dll", "msvcrt.dll", "mswsock.dll",
    "netapi32.dll", "normaliz.dll", "ntdll.dll", "ole32.dll", "oleaut32.dll",
    "psapi.dll", "rpcrt4.dll", "secur32.dll", "setupapi.dll", "shell32.dll",
    "shlwapi.dll", "ucrtbase.dll", "user32.dll", "userenv.dll", "version.dll",
    "winhttp.dll", "winmm.dll", "winspool.drv", "wldap32.dll", "ws2_32.dll",
    "wsock32.dll",
}
PLACEHOLDER = b"0000000001000000000100000000010000000001"


def package(source: Path, destination: Path, revision: str, objdump: str) -> None:
    destination.mkdir(parents=True)
    for file in source.iterdir():
        if file.suffix.lower() in {".exe", ".dll"}:
            # Dereference Nix store symlinks: the ZIP must be self-contained.
            shutil.copyfile(file, destination / file.name)

    executable = destination / "hydra-node.exe"
    content = executable.read_bytes()
    if content.count(PLACEHOLDER) != 1 or len(revision) > len(PLACEHOLDER):
        raise ValueError("Expected exactly one revision placeholder of sufficient length")
    executable.write_bytes(content.replace(PLACEHOLDER, revision.ljust(40).encode("ascii")))

    bundled = {file.name.lower() for file in destination.iterdir()}
    for file in destination.iterdir():
        description = subprocess.check_output([objdump, "-p", str(file)], text=True)
        if "file format pei-x86-64" not in description:
            raise ValueError(f"Not a Windows x86_64 executable: {file}")
        imports = re.findall(r"DLL Name:\s*(\S+)", description)
        for name in imports:
            name = name.lower()
            if name not in bundled | SYSTEM_DLLS and not name.startswith(("api-ms-win-", "ext-ms-win-")):
                raise ValueError(f"{file.name} needs unbundled DLL {name}")


if __name__ == "__main__":
    package(Path(sys.argv[1]), Path(sys.argv[2]), sys.argv[3], sys.argv[4])
