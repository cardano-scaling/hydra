# A set of buildables we typically build for releases

{ hydraProject # as defined in default.nix
, system ? builtins.currentSystem
, pkgs
, cardano-node
, gitRev ? "unknown"
}:
let
  nativePkgs = hydraProject.hsPkgs;
  # Allow reinstallation of terminfo as it's not installed with cross compilers.
  patchedForCrossProject = hydraProject.hsPkgs.appendModule
    ({ lib, ... }: { options.nonReinstallablePkgs = lib.mkOption { apply = lib.remove "terminfo"; }; });
  musl64Pkgs = patchedForCrossProject.projectCross.musl64.hsPkgs;
in
rec {
  hydra-node = nativePkgs.hydra-node.components.exes.hydra-node;
  hydra-node-static = musl64Pkgs.hydra-node.components.exes.hydra-node;

  hydra-tools-static = musl64Pkgs.hydra-node.components.exes.hydra-tools;

  hydra-tui = nativePkgs.hydra-tui.components.exes.hydra-tui;
  hydra-tui-static = musl64Pkgs.hydra-tui.components.exes.hydra-tui;

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
      buildInputs = [ nativePkgs.hydra-node.components.tests.tests ];
    };
    hydra-cluster = pkgs.mkShellNoCC {
      name = "hydra-cluster-tests";
      buildInputs =
        [
          nativePkgs.hydra-cluster.components.tests.tests
          hydra-node
          cardano-node.packages.${system}.cardano-node
        ];
    };
    hydra-tui = pkgs.mkShellNoCC {
      name = "hydra-tui-tests";
      buildInputs =
        [
          nativePkgs.hydra-tui.components.tests.tests
          hydra-node
          cardano-node.packages.${system}.cardano-node
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
          cardano-node.packages.${system}.cardano-node
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
