{
  description = "Hydra project flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-2009";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix }:
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs;
      name = "hydra-node";
      shell = ./shell.nix;
      preOverlays = [ haskell-nix.overlay ];
    };
}
