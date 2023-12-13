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
    cardano-node.url = "github:input-output-hk/cardano-node/8.7.2";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , cardano-node
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
          inherit hydraProject system pkgs cardano-node;
          gitRev = self.rev or "dirty";
        };
        hydraImages = import ./nix/hydra/docker.nix {
          inherit hydraPackages system nixpkgs;
        };
        prefixAttrs = s: attrs:
          with pkgs.lib.attrsets;
          mapAttrs' (name: value: nameValuePair (s + name) value) attrs;
      in
      rec {
        inherit hydraProject;

        packages =
          { default = hydraPackages.hydra-node; } //
          hydraPackages //
          prefixAttrs "docker-" hydraImages // {
            spec = import ./spec { inherit pkgs; };
          };

        devShells = (import ./nix/hydra/shell.nix {
          inherit (inputs) cardano-node;
          inherit hydraProject system;
        }) // {
          ci = (import ./nix/hydra/shell.nix {
            inherit (inputs) cardano-node;
            inherit hydraProject system;
            withoutDevTools = true;
          }).default;
        };

        # Build selected derivations in CI for caching
        hydraJobs = {
          packages = { inherit (packages) hydra-node hydra-tui hydraw spec; };
          devShells = { inherit (devShells) default ci; };
        };
      });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://hydra-node.cachix.org"
      "https://cardano-scaling.cachix.org"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "hydra-node.cachix.org-1:vK4mOEQDQKl9FTbq76NjOuNaRD4pZLxi1yri31HHmIw="
      "cardano-scaling.cachix.org-1:RKvHKhGs/b6CBDqzKbDk0Rv6sod2kPSXLwPzcUQg9lY="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
  };
}
