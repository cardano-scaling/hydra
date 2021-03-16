# Alternative set up using nix flakes. Flakes are still considered experimental
# and also the fact, that we do not have a proper cache set up for all this, is
# why you would still want to use the shell.nix instead.
#
# See https://nixos.wiki/wiki/Flakes for more info
{
  description = "Hydra project flake";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            # REVIEW(SN): why an overlay?
            hydraProject =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8104";
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; };
        flake = pkgs.hydraProject.flake { };
      in
      flake // {
        # Built by `nix build`
        defaultPackage = flake.packages."local-cluster:exe:local-cluster";

        # Shell provided to `nix develop`
        devShell = pkgs.hydraProject.shellFor {
          tools = {
            cabal = "latest";
            hlint = "latest";
            haskell-language-server = "latest";
          };
        };
      });
}
