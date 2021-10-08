{ compiler ? "ghc8107"
, system ? builtins.currentSystem
, haskellNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/haskell.nix/archive/3a467be1727c824c949536be8e98400191e6fd72.tar.gz")
    { }
, iohkNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/iohk-nix/archive/dc03db8b910aabba75931e524368df14eb9f7852.tar.gz")
  { }
, cardanoNodePkgs ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/cardano-node/archive/8fe46140a52810b6ca456be01d652ca08fe730bf.tar.gz")
    { gitrev = "8fe46140a52810b6ca456be01d652ca08fe730bf"; }
# check https://input-output-hk.github.io/haskell.nix/reference/supported-ghc-versions/#supported-ghc-versions
# when changing ghc version, to update nixpkgs version
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2105
, nixpkgsArgs ? haskellNix.nixpkgsArgs
}:
let
  pkgs = import nixpkgsSrc (nixpkgsArgs // {
    overlays =
      # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
      haskellNix.overlays
        # needed for cardano-api wich uses a patched libsodium
        ++ iohkNix.overlays.crypto;
  });
in
{
  inherit pkgs;
  cardano-node = cardanoNodePkgs;
  hydra = pkgs.haskell-nix.cabalProject' {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "hydra-poc";
      src = ./.;
    };
    compiler-nix-name = compiler;

    # Fixed output derivation for plan-nix
    materialized = ./nix/hydra-poc.materialized;
    # Enable this and nix-build one of the project components to get the new
    # plan-sha256 and materialization update scripts:
    # checkMaterialization = true;

    modules = [{
      packages = {
        eventful-sql-common = {
          # This is needed so evenful-sql-common will build with a newer version of persistent.
          ghcOptions = [ "-XDerivingStrategies -XStandaloneDeriving -XUndecidableInstances -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses" ];
          doHaddock = false;
        };

        # Broken due to haddock errors. Refer to https://github.com/input-output-hk/plutus/blob/master/nix/pkgs/haskell/haskell.nix
        plutus-ledger.doHaddock = false;
        plutus-use-cases.doHaddock = false;

        # See https://github.com/input-output-hk/iohk-nix/pull/488
        cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
        cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];

      };
    }];
  };
}
