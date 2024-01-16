# A set of buildables we typically build for releases

{ hydraProject # as defined in default.nix
, system ? builtins.currentSystem
, pkgs
, inputs
, gitRev ? "unknown"
}:
let
  lib = pkgs.lib;

  packaging = import ../packaging.nix { inherit pkgs; };

  # Creates a fixed length string by padding with given filler as suffix.
  padSuffix = width: filler: str:
    let
      strw = lib.stringLength str;
      reqWidth = width - (lib.stringLength filler);
    in
    assert lib.assertMsg (strw <= width)
      "padSuffix: requested string length (${toString width}) must not be shorter than actual length (${toString strw})";
    if strw == width then str else padSuffix reqWidth filler str + filler;

  paddedRevision = padSuffix 40 " " gitRev;

  # Placeholder used to 'embedRevision'. See also hydra-prelude/cbits/revision.c
  placeholder = "0000000001000000000100000000010000000001";

  # Takes a derivation with a single executable and embeds the given revision
  # into this executable.
  embedRevision = drv: exe: rev:
    assert lib.assertMsg
      (lib.stringLength placeholder == lib.stringLength rev)
      "Mismatching length of placeholder (${placeholder}) and rev (${rev})";

    # Using mkDerivation instead of runCommand to make sure to use the same
    # stdenv as the original drv (important to determine targetPlatform).
    drv.stdenv.mkDerivation {
      name = "${exe}-with-revision";
      phases = [ "buildPhase" ];
      buildPhase = ''
        set -e

        echo "Patching embedded git revision in ${exe} to ${rev} ..."

        # Ensure only one occurrence of placeholder
        if [[ $(grep -c -a ${placeholder} ${drv}/bin/${exe}) -ne 1 ]]; then
          echo "Not exactly one occurrence of ${placeholder} in ${drv}/bin/${exe}!"
          exit 1
        fi

        mkdir -p $out/bin
        sed 's/${placeholder}/${rev}/' ${drv}/bin/${exe} > $out/bin/${exe}
        chmod +x $out/bin/${exe}
      '';
    };

  nativePkgs = hydraProject.hsPkgs;
  # Allow reinstallation of terminfo as it's not installed with cross compilers.
  patchedForCrossProject = hydraProject.hsPkgs.appendModule
    ({ lib, ... }: { options.nonReinstallablePkgs = lib.mkOption { apply = lib.remove "terminfo"; }; });
  musl64Pkgs = patchedForCrossProject.projectCross.musl64.hsPkgs;
in
rec {
  release =
    packaging.asZip
      { name = "hydra-${pkgs.hostPlatform.system}"; }
      [ hydra-node hydra-tui ];

  release-static =
    packaging.asZip
      { name = "hydra-${pkgs.hostPlatform.system}"; }
      [ hydra-node-static hydra-tui-static ];

  hydra-node =
    embedRevision
      nativePkgs.hydra-node.components.exes.hydra-node
      "hydra-node"
      paddedRevision;

  hydra-node-static =
    embedRevision
      musl64Pkgs.hydra-node.components.exes.hydra-node
      "hydra-node"
      paddedRevision;

  hydra-chain-observer =
    nativePkgs.hydra-chain-observer.components.exes.hydra-chain-observer;

  hydra-chain-observer-static =
    musl64Pkgs.hydra-chain-observer.components.exes.hydra-chain-observer;

  hydra-tui =
    embedRevision
      nativePkgs.hydra-tui.components.exes.hydra-tui
      "hydra-tui"
      paddedRevision;

  hydra-tui-static =
    embedRevision
      musl64Pkgs.hydra-tui.components.exes.hydra-tui
      "hydra-tui"
      paddedRevision;

  hydraw = nativePkgs.hydraw.components.exes.hydraw;

  hydraw-static = musl64Pkgs.hydraw.components.exes.hydraw;

  tests = {
    plutus-cbor = pkgs.mkShellNoCC {
      name = "plutus-cbor-tests";
      buildInputs = [ nativePkgs.plutus-cbor.components.tests.tests ];
    };
    plutus-merkle-tree = pkgs.mkShellNoCC {
      name = "plutus-merkle-tree-tests";
      buildInputs = [ nativePkgs.plutus-merkle-tree.components.tests.tests ];
    };
    hydra-plutus = pkgs.mkShellNoCC {
      name = "hydra-plutus-tests";
      buildInputs = [ nativePkgs.hydra-plutus.components.tests.tests ];
    };
    hydra-node = pkgs.mkShellNoCC {
      name = "hydra-node-tests";
      buildInputs = [
        nativePkgs.hydra-node.components.tests.tests
        pkgs.check-jsonschema
      ];
    };
    hydra-cluster = pkgs.mkShellNoCC {
      name = "hydra-cluster-tests";
      buildInputs =
        [
          nativePkgs.hydra-cluster.components.tests.tests
          hydra-node
          hydra-chain-observer
          inputs.cardano-node.packages.${system}.cardano-node
          inputs.cardano-node.packages.${system}.cardano-cli
          inputs.mithril.packages.${system}.mithril-client-cli
          pkgs.check-jsonschema
        ];
    };
    hydra-tui = pkgs.mkShellNoCC {
      name = "hydra-tui-tests";
      buildInputs =
        [
          nativePkgs.hydra-tui.components.tests.tests
          hydra-node
          inputs.cardano-node.packages.${system}.cardano-node
        ];
    };
  };

  benchs = {
    hydra-node = pkgs.mkShellNoCC {
      name = "bench-hydra-node";
      buildInputs = [
        nativePkgs.hydra-node.components.benchmarks.tx-cost
        nativePkgs.hydra-node.components.benchmarks.micro
      ];
    };
    hydra-cluster = pkgs.mkShellNoCC {
      name = "bench-hydra-cluster";
      buildInputs =
        [
          nativePkgs.hydra-cluster.components.benchmarks.bench-e2e
          hydra-node
          inputs.cardano-node.packages.${system}.cardano-node
          inputs.cardano-node.packages.${system}.cardano-cli
        ];
    };
    plutus-merkle-tree = pkgs.mkShellNoCC {
      name = "bench-plutus-merkle-tree";
      buildInputs = [ nativePkgs.plutus-merkle-tree.components.benchmarks.on-chain-cost ];
    };
  };

  haddocks = pkgs.runCommand "hydra-haddocks"
    {
      paths = [
        hydraProject.hsPkgs.plutus-cbor.components.library.doc
        hydraProject.hsPkgs.plutus-merkle-tree.components.library.doc
        hydraProject.hsPkgs.hydra-prelude.components.library.doc
        hydraProject.hsPkgs.hydra-cardano-api.components.library.doc
        hydraProject.hsPkgs.hydra-plutus.components.library.doc
        hydraProject.hsPkgs.hydra-node.components.library.doc
        hydraProject.hsPkgs.hydra-node.components.tests.tests.doc
        hydraProject.hsPkgs.hydra-cluster.components.library.doc
        hydraProject.hsPkgs.hydra-tui.components.library.doc
      ];
    }
    ''
      set -ex
      mkdir -p $out
      for p in $paths; do
        cd $p
        for html in $(find $haddockRoot -name html -type d); do
          package=$(basename $(dirname $html))
          mkdir -p $out/$package
          cp -a $html/* $out/$package/
        done
      done
    '';
}
