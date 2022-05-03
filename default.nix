{ compiler ? "ghc8107"
, system ? builtins.currentSystem
, haskellNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/haskell.nix/archive/28dbf2f4bd32a4fbd1a2e9de45d02ad977b062d9.tar.gz")
    { }
, iohkNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/iohk-nix/archive/62d853d3216083ecadc8e7f192498bebad4eee76.tar.gz")
    { }
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2111
, nixpkgsArgs ? haskellNix.nixpkgsArgs
}:
let
  pkgs = import nixpkgsSrc (nixpkgsArgs // {
    overlays =
      # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
      haskellNix.overlays
      # needed for cardano-api which uses a patched libsodium
      ++ iohkNix.overlays.crypto;
  });
in
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "hydra-poc";
    src = ./.;
  };
  projectFileName = "cabal.project";
  compiler-nix-name = compiler;

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

    # https://github.com/input-output-hk/cardano-wallet/commit/ced95e1b84ce8d9faa53268be45e96701ccc16e9
    packages.cardano-config.components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitMinimal ];

    # https://github.com/input-output-hk/iohk-nix/pull/488
    packages.cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
    packages.cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
  }];
}
