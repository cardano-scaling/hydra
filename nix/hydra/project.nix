{ compiler ? "ghc8107"

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
      haskellNix.overlay
      iohk-nix.overlays.crypto
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
      # Set libsodium-vrf on cardano-crypto-{praos,class}. Otherwise they depend
      # on libsodium, which lacks the vrf functionality.
      ({ pkgs, lib, ... }:
        # Override libsodium with local 'pkgs' to make sure it's using
        # overriden 'pkgs', e.g. musl64 packages
        {
          packages.cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
          packages.cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
        }
      )
    ];
  };
in
{
  inherit compiler pkgs hsPkgs;
}
