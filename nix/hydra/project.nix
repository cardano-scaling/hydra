{ compiler ? "ghc8107"

, system ? builtins.currentSystem

, haskellNix

, iohk-nix

, CHaP

, nixpkgs ? iohk-nix.nixpkgs

, gitRev ? ""
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
    # TODO: probably should use flake.nix inputs.self here
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "hydra";
      src = ./../..;
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
      {
        packages.hydra-node.preBuild = ''
          echo ======= PATCHING --version to ${gitRev} =======
          export GIT_REVISION=${gitRev}
        '';
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
