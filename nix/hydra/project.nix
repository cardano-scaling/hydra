{ compiler ? "ghc963"

, system ? builtins.currentSystem

, haskellNix

, iohk-nix

, CHaP

, nixpkgs ? iohk-nix.nixpkgs
}:
let
  # nixpkgs enhanced with haskell.nix and crypto libs as used by iohk
  pkgs = import nixpkgs {
    inherit system;
    overlays = [
      # This overlay contains libsodium and libblst libraries
      iohk-nix.overlays.crypto
      # This overlay contains pkg-config mappings via haskell.nix to use the
      # crypto libraries above
      iohk-nix.overlays.haskell-nix-crypto
      # Keep haskell.nix as the last overlay!
      #
      # Reason: haskell.nix modules/overlays neds to be last
      # https://github.com/input-output-hk/haskell.nix/issues/1954
      haskellNix.overlay
    ];
  };

  hsPkgs = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
      name = "hydra";
      src = ./../..;
      filter = path: type:
        # Blacklist of paths which do not affect the haskell build. The smaller
        # the resulting list of files is, the less likely we have redundant
        # rebuilds.
        builtins.all (x: baseNameOf path != x) [
          "flake.nix"
          "flake.lock"
          "nix"
          ".github"
          "demo"
          "docs"
          "sample-node-config"
          "spec"
          "testnets"
        ];
    };
    projectFileName = "cabal.project";
    compiler-nix-name = compiler;

    inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };

    modules = [{
      packages = {

        # Strip debugging symbols from exes (smaller closures)
        hydra-node.dontStrip = false;
        hydra-tui.dontStrip = false;
        hydraw.dontStrip = false;

        # Avoid plutus-tx errors in haddock (see also cabal.project)
        hydra-plutus.setupHaddockFlags = [ "--ghc-options='-fplugin-opt PlutusTx.Plugin:defer-errors'" ];

        plutus-merkle-tree.setupHaddockFlags = [ "--ghc-options='-fplugin-opt PlutusTx.Plugin:defer-errors'" ];
        plutus-cbor.setupHaddockFlags = [ "--ghc-options='-fplugin-opt PlutusTx.Plugin:defer-errors'" ];

        # Fix compliation of strict-containers (see also cabal.project)
        strict-containers.ghcOptions = [ "-Wno-noncanonical-monad-instances" ];

        # XXX: Could not figure out where to make this flag ^^^ effective in the haddock build
        strict-containers.doHaddock = false;

        # -Werror for CI

        hydra-cardano-api.ghcOptions = [ "-Werror" ];
        hydra-cluster.ghcOptions = [ "-Werror" ];
        hydra-node.ghcOptions = [ "-Werror" ];
        hydra-plutus.ghcOptions = [ "-Werror" ];
        hydra-plutus-extras.ghcOptions = [ "-Werror" ];
        hydra-prelude.ghcOptions = [ "-Werror" ];
        hydra-test-utils.ghcOptions = [ "-Werror" ];
        hydra-tui.ghcOptions = [ "-Werror" ];
        hydraw.ghcOptions = [ "-Werror" ];
        plutus-cbor.ghcOptions = [ "-Werror" ];
        plutus-merkle-tree.ghcOptions = [ "-Werror" ];

      };
    }
      ({ lib, config, ... }:
        lib.mkIf (lib.versionAtLeast config.compiler.version "9.4") {
          # lib:ghc is a bit annoying in that it comes with it's own build-type:Custom, and then tries
          # to call out to all kinds of silly tools that GHC doesn't really provide.
          # For this reason, we try to get away without re-installing lib:ghc for now.
          reinstallableLibGhc = false;
        })];
  };
in
{
  inherit compiler pkgs hsPkgs;
}
