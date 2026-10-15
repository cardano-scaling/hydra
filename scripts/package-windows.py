#!/usr/bin/env python3
"""Collect a Windows release with its runtime licenses and provenance."""

import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tarfile


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


def sha256(path: Path) -> str:
    with path.open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def runtime_notices(destination: Path, manifest: Path, repository: Path, revision: str, dlls):
    data = json.loads(manifest.read_text())
    records = data["packages"] + [data["mingw"]]
    for record in records:
        record["dlls"] = {}
    for dll in dlls:
        resolved = dll.resolve()
        owners = [r for r in data["packages"] if resolved.is_relative_to(r["output"])]
        if len(owners) != 1:
            raise ValueError(f"No unique license/provenance for {dll.name}: {resolved}")
        owners[0]["dlls"][dll.name] = {
            "sha256": sha256(destination / dll.name), "original": str(resolved),
        }

    for record in records:
        name = f"{record['pname']}-{record['version']}"
        licenses = destination / "licenses" / name
        licenses.mkdir(parents=True)
        source = Path(record["source"])
        if source.is_dir():
            for relative in record["licenseFiles"]:
                original, target = source / relative, licenses / relative
                if original.is_dir():
                    shutil.copytree(original, target)
                else:
                    shutil.copyfile(original, target)
        else:
            # Read only the licenses without unpacking the complete source tree.
            found = set()
            with tarfile.open(source) as tar:
                for member in tar:
                    relative = Path(member.name).relative_to(member.name.split("/")[0])
                    for requested in record["licenseFiles"]:
                        if member.isfile() and relative.is_relative_to(requested):
                            target = licenses / relative
                            target.parent.mkdir(parents=True, exist_ok=True)
                            with tar.extractfile(member) as original, target.open("wb") as output:
                                shutil.copyfileobj(original, output)
                            found.add(requested)
            if found != set(record["licenseFiles"]):
                raise ValueError(f"Missing licenses for {name}: {set(record['licenseFiles']) - found}")
    for name in ("LICENSE", "NOTICE"):
        shutil.copyfile(repository / name, destination / name)
    data["hydraRevision"] = revision
    (destination / "runtime-provenance.json").write_text(json.dumps(data, indent=2, sort_keys=True) + "\n")
    lines = [
        "Hydra Windows runtime libraries", "===============================", "",
        "Keep LICENSE, NOTICE, licenses/ and runtime-provenance.json with redistributions.",
        "The following libraries retain their own licenses; full texts and copyright notices",
        "are copied from the exact pinned sources into licenses/. No warranty is provided.", "",
    ]
    for record in records:
        lines.append(f"{record['pname']} {record['version']}: {record['license']}")
        lines.append("  " + (", ".join(sorted(record["dlls"])) or "MinGW-w64 code linked into the binaries"))
        lines.append(f"  Upstream: {record['upstream']}")
    lines += [
        "", "runtime-provenance.json records package versions, source URLs/revisions and hashes,",
        "Nix build-input commits and hashes, Nix outputs, and SHA-256 hashes of shipped DLLs.",
        "Source archives, patches and build recipes are not included in this ZIP.",
        "Provenance records do not replace the licenses' corresponding-source requirements;",
        "redistributors must arrange source access separately for GPL/LGPL-covered libraries.",
        "", "To rebuild a runtime on x86_64 Linux with Nix, check out the Hydra revision in",
        "runtime-provenance.json and use its flake.lock (a 'dirty' revision denotes local changes):",
        "  nix build .#legacyPackages.x86_64-linux.pkgsCross.ucrt64.<attribute>",
        "Use the package's attribute from runtime-provenance.json. Nix fetches the pinned sources.",
        "", "GMP is dynamically linked. You may replace its DLL with an interface-compatible",
        "modified version. Nothing here restricts modification or reverse engineering for",
        "debugging modifications to LGPL-covered libraries. GCC and mcfgthread include",
        "the GCC Runtime Library Exception.",
        "", "This inventory covers the shipped DLLs and the MinGW runtime. It is not a full",
        "inventory of the Haskell or embedded etcd dependency graphs.", "",
    ]
    (destination / "WINDOWS-RUNTIME-NOTICES.txt").write_text("\n".join(lines))


def package(source: Path, destination: Path, revision: str, objdump: str, manifest: Path, repository: Path) -> None:
    destination.mkdir(parents=True)
    available = {file.name.lower(): file for file in source.iterdir() if file.suffix.lower() == ".dll"}
    pending = [source / "hydra-node.exe"]
    bundled = {}
    while pending:
        file = pending.pop()
        if file.name.lower() in bundled:
            continue
        description = subprocess.check_output([objdump, "-p", str(file)], text=True)
        if "file format pei-x86-64" not in description:
            raise ValueError(f"Not a Windows x86_64 executable: {file}")
        bundled[file.name.lower()] = file
        for name in re.findall(r"DLL Name:\s*(\S+)", description):
            name = name.lower()
            if name in SYSTEM_DLLS or name.startswith(("api-ms-win-", "ext-ms-win-")):
                continue
            if name not in available:
                raise ValueError(f"{file.name} needs unbundled DLL {name}")
            pending.append(available[name])
        # Dereference Nix store symlinks: the ZIP must be self-contained.
        shutil.copyfile(file, destination / file.name)

    executable = destination / "hydra-node.exe"
    content = executable.read_bytes()
    if content.count(PLACEHOLDER) != 1 or len(revision) > len(PLACEHOLDER):
        raise ValueError("Expected exactly one revision placeholder of sufficient length")
    executable.write_bytes(content.replace(PLACEHOLDER, revision.ljust(40).encode("ascii")))

    runtime_notices(destination, manifest, repository, revision,
                    [file for file in bundled.values() if file.suffix.lower() == ".dll"])


if __name__ == "__main__":
    package(Path(sys.argv[1]), Path(sys.argv[2]), sys.argv[3], sys.argv[4], Path(sys.argv[5]), Path(sys.argv[6]))
