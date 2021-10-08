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
  # nixpkgs 21.05 at 2021-07-19
, pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/4181644d09b96af0f92c2f025d3463f9d19c7790.tar.gz") { }
# check https://input-output-hk.github.io/haskell.nix/reference/supported-ghc-versions/#supported-ghc-versions
# when changing ghc version, to update nixpkgs version
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2105
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, libsodium-vrf ? pkgs.libsodium.overrideAttrs (oldAttrs: {
    name = "libsodium-1.0.18-vrf";
    src = pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "libsodium";
      # branch tdammers/rebased-vrf
      rev = "b397839b58ccfd09dde2191c7e1b67d47184f6b0";
      sha256 = "12g2wz3gyi69d87nipzqnq4xc6nky3xbmi2i2pb2hflddq8ck72f";
    };
    nativeBuildInputs = [ pkgs.autoreconfHook ];
    configureFlags = "--enable-static";
  })
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
  projectFileName = "cabal.project";
  compiler-nix-name = compiler;

  # Fixed output derivation for plan-nix
  plan-sha256 = "0fp9zc2nl4zq4lbbnm78i8giw3jdqj1y3wy933f0kqlsyhd2a72y";
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
      cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ libsodium-vrf ] ];
      cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ libsodium-vrf ] ];

    };
  }];
}
