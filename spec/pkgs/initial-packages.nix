{ pkgs, lib, newScope, Agda, nixpkgs }:

let
  mkAgdaPackages = Agda: lib.makeScope newScope (mkAgdaPackages' Agda);
  mkAgdaPackages' = Agda: self:
    let
      inherit (self) callPackage;
      inherit (callPackage "${nixpkgs}/pkgs/build-support/agda" {
        inherit Agda self;
        inherit (pkgs.haskellPackages) ghcWithPackages;
      }) withPackages mkDerivation;
    in
    {
      inherit withPackages mkDerivation;

      lib = lib.extend (_final: prev: import "${nixpkgs}/pkgs/build-support/agda/lib.nix" { lib = prev; });

      agda = withPackages [ ];

      abstract-set-theory = callPackage ./abstract-set-theory.nix { };

      formal-ledger = callPackage ./formal-ledger.nix { };

      standard-library = callPackage ./standard-library.nix { };
      inherit (pkgs.haskellPackages) ghcWithPackages;

      standard-library-classes = callPackage ./standard-library-classes.nix { };

      standard-library-meta = callPackage ./standard-library-meta.nix { };

    };
in
mkAgdaPackages Agda
