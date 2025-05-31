{ inputs, ... }: {

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
