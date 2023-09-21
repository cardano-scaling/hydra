{ compiler ? "ghc928"

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

    modules = [
      # Strip debugging symbols from exes (smaller closures)
      {
        packages.hydra-node.dontStrip = false;
        packages.hydra-tui.dontStrip = false;
        packages.hydraw.dontStrip = false;
      }
      # Avoid plutus-tx errors in haddock (see also cabal.project)
      {
        packages.hydra-plutus.setupHaddockFlags = [ "--ghc-options='-fplugin-opt PlutusTx.Plugin:defer-errors'" ];
        packages.plutus-merkle-tree.setupHaddockFlags = [ "--ghc-options='-fplugin-opt PlutusTx.Plugin:defer-errors'" ];
        packages.plutus-cbor.setupHaddockFlags = [ "--ghc-options='-fplugin-opt PlutusTx.Plugin:defer-errors'" ];
      }
      # Fix compliation of strict-containers (see also cabal.project)
      {
        packages.strict-containers.ghcOptions = [ "-Wno-noncanonical-monad-instances" ];
        # XXX: Could not figure out where to make this flag ^^^ effective in the haddock build
        packages.strict-containers.doHaddock = false;
      }
    ];
  };
in
{
  inherit compiler pkgs hsPkgs;
}
