{ compiler ? "ghc964"

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
      # Custom static libs used for darwin build
      (import ../static-libs.nix)
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
      # Fix compliation of strict-containers (see also cabal.project)
      {
        packages.strict-containers.ghcOptions = [ "-Wno-noncanonical-monad-instances" ];
        # XXX: Could not figure out where to make this flag ^^^ effective in the haddock build
        packages.strict-containers.doHaddock = false;
      }
      # Use different static libs on darwin
      # TODO: Always use these?
      (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
        packages.hydra-node.ghcOptions = with pkgs; [
          "-L${lib.getLib static-gmp}/lib"
          "-L${lib.getLib static-libsodium-vrf}/lib"
          "-L${lib.getLib static-secp256k1}/lib"
          "-L${lib.getLib static-openssl}/lib"
          "-L${lib.getLib static-libblst}/lib"
        ];
      })
      # Fix compilation with newer ghc versions
      ({ lib, config, ... }:
        lib.mkIf (lib.versionAtLeast config.compiler.version "9.4") {
          # lib:ghc is a bit annoying in that it comes with it's own build-type:Custom, and then tries
          # to call out to all kinds of silly tools that GHC doesn't really provide.
          # For this reason, we try to get away without re-installing lib:ghc for now.
          reinstallableLibGhc = false;
        })
    ];
  };
in
{
  inherit compiler pkgs hsPkgs;
}
