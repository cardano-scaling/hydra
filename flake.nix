{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:numtide/flake-utils";
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , ...
    } @ inputs:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ]
      (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };
        hydraProject = import ./nix/hydra/project.nix {
          inherit (inputs) haskellNix iohk-nix CHaP;
          inherit system nixpkgs;
        };
        hydraPackages = import ./nix/hydra/packages.nix {
          inherit hydraProject system;
        };
        hydraImages = import ./nix/hydra/docker.nix {
          inherit hydraPackages system nixpkgs;
        };
        prefixAttrs = s: attrs:
          with pkgs.lib.attrsets;
          mapAttrs' (name: value: nameValuePair (s + name) value) attrs;
      in
      rec {
        packages =
          hydraPackages //
          prefixAttrs "docker-" hydraImages;

        devShells = (import ./nix/hydra/shell.nix {
          inherit hydraProject system;
        }) // {
          ci = (import ./nix/hydra/shell.nix {
            inherit hydraProject system;
            withoutDevTools = true;
          }).default;
        };

        # Build all derivations in CI for caching
        hydraJobs = { inherit packages devShells; };
      });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://hydra-node.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "hydra-node.cachix.org-1:vK4mOEQDQKl9FTbq76NjOuNaRD4pZLxi1yri31HHmIw="
    ];
    allow-import-from-derivation = true;
  };
}
