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
      (flake-utils.lib.eachSystem [ "x86_64-linux" ]
        (system:
        let
          hydraProject = import ./nix/hydra/project.nix {
            inherit (inputs) nixpkgs haskellNix iohk-nix CHaP;
            inherit system;
          };
        in
        {
          packages = import ./nix/hydra/packages.nix {
            inherit hydraProject system;
          };

          devShells = import ./nix/hydra/shell.nix {
            inherit hydraProject system;
          };
        })
      );

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = true;
  };
}
