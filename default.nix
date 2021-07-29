{ compiler ? "ghc8104"
, system ? builtins.currentSystem
, haskellNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/haskell.nix/archive/f9d261d6d90d4aebd97f7ae60c951fc9e1d98493.tar.gz")
    { }
, iohkNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/iohk-nix/archive/7f57f750e27c84def8e0ed2492eea01ea957cbf2.tar.gz")
    { }
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009
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
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "hydra-poc";
    src = ./.;
  };
  compiler-nix-name = compiler;

  # Fixed output derivation for plan-nix
  plan-sha256 = "0ziwy71jf7l9mr20r845zjwc6ri5y161hn4fzkif3lqlfiygb1ak";
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
    };
  }];
}
