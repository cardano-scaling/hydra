{ inputs, ... }: {

  imports = [
    inputs.process-compose-flake.flakeModule
  ];

  systems = [
    "x86_64-linux"
    "x86_64-darwin"
    "aarch64-darwin"
    "aarch64-linux"
  ];

  perSystem = { pkgs, hsPkgs, config, lib, system, ... }:
    let
      compiler = "ghc966";

      inputMap = { "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP; };

    in
    {

      _module.args = { inherit compiler inputMap; };

      legacyPackages = pkgs // hsPkgs;

    };
}
