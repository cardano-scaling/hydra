{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:numtide/flake-utils";
    std.follows = "tullia/std";
    tullia.url = github:input-output-hk/tullia;
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs =
    { self
    , std
    , tullia
    , flake-utils
    , nixpkgs
    , ...
    } @ inputs:
    std.growOn
      {
        inherit inputs;
        cellsFrom = ./nix;
        cellBlocks = [
          (std.functions "library")
          (std.functions "hydraJobs")
          (tullia.tasks "pipelines")
          (std.functions "actions")
        ];
      }
      (
        tullia.fromStd {
          actions = std.harvest self [ "cloud" "actions" ];
          tasks = std.harvest self [ "automation" "pipelines" ];
        }
      )
      (flake-utils.lib.eachSystem [
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
        in
        rec {
          packages = hydraPackages // {
            docker = hydraImages;
          };

          devShells = (import ./nix/hydra/shell.nix {
            inherit hydraProject system;
          }) // {
            ci = (import ./nix/hydra/shell.nix {
              inherit hydraProject system;
              withoutDevTools = true;
            }).default;
          };

        })
      );

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
