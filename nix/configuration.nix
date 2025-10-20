{ inputs, ... }: {

  imports = [
    inputs.hydra-coding-standards.flakeModule
    inputs.process-compose-flake.flakeModule
  ];

  perSystem = { pkgs, hsPkgs, system, ... }:
    let
      compiler = "ghc967";
      inputMap = { "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP; };
      pkgs-2411 = import inputs.nixpkgs-2411 { inherit system; };
    in
    {
      _module.args = { inherit compiler inputMap pkgs-2411; };
      legacyPackages = pkgs // hsPkgs;
    };
}
