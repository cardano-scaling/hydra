#!/usr/bin/env python3
"""Use the scripts compiled by the native build in a Windows source copy."""

from pathlib import Path
import re
import sys


DEFINITIONS = {
    "Head": {"unappliedValidator": ("code", "head.flat")},
    "HeadTokens": {"unappliedMintingPolicy": ("code", "mint.flat")},
    "CRS": {"crsValidatorScript": ("code", "crs.flat")},
    "Dummy": {
        "dummyValidatorScript": ("script", "dummy-validator.cbor"),
        "dummyValidatorScriptAlwaysFails": ("script", "dummy-failing.cbor"),
        "dummyMintingScript": ("script", "dummy-minting.cbor"),
        "dummyRewardingScript": ("script", "dummy-rewarding.cbor"),
        "exampleSecureValidatorScript": ("script", "dummy-secure.cbor"),
    },
}


def prepare(source: Path) -> None:
    generated = [
        "{-# LANGUAGE TemplateHaskell #-}",
        "{-# LANGUAGE NoImplicitPrelude #-}",
        "module Hydra.Contract.Precompiled where",
        "import Prelude",
        "import Data.ByteString.Short (toShort)",
        "import Data.FileEmbed (embedFile)",
        "import Hydra.Cardano.Api (PlutusScript, pattern PlutusScriptSerialised)",
        "import PlutusTx.Code (CompiledCode, CompiledCodeIn (SerializedCode))",
    ]
    for module, definitions in DEFINITIONS.items():
        path = source / "Hydra" / "Contract" / f"{module}.hs"
        text = path.read_text()
        text = text.replace("{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}\n", "")
        text, count = re.subn(r"(?m)^import ", "import Hydra.Contract.Precompiled qualified as Precompiled\nimport ", text, count=1)
        assert count == 1, f"Could not insert import in {path}"
        for name, (kind, artifact) in definitions.items():
            text, count = re.subn(
                rf"(?ms)^{name} =\n.*?(?=\n\n|\Z)",
                f"{name} = Precompiled.{name}", text,
            )
            assert count == 1, f"Could not replace {module}.{name}"
            if kind == "code":
                generated += [f"{name} :: CompiledCode a",
                              f'{name} = SerializedCode $(embedFile "cross-scripts/{artifact}") (Just $(embedFile "cross-scripts/{artifact}.pir")) mempty']
            else:
                generated += [f"{name} :: PlutusScript",
                              f'{name} = PlutusScriptSerialised (toShort $(embedFile "cross-scripts/{artifact}"))']
        path.write_text(text)
    for path in source.rglob("*.hs"):
        assert not re.search(r"\bPlutusTx\.compile\b", path.read_text()), f"Unexported script in {path}"
    (source / "Hydra" / "Contract" / "Precompiled.hs").write_text("\n".join(generated) + "\n")


if __name__ == "__main__":
    prepare(Path(sys.argv[1]))
