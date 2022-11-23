{
  inputs = {
    nixpkgs.follows = "tullia/nixpkgs";
    std.follows = "tullia/std";
    tullia.url = github:input-output-hk/tullia;
  };

  outputs = {
    self,
    std,
    tullia,
    ...
  } @ inputs:
    std.growOn {
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
        actions = std.harvest self ["cloud" "actions"];
        tasks = std.harvest self ["automation" "pipelines"];
      }
    )
    {
      hydraJobs = std.harvest self ["automation" "hydraJobs"];
    };

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = true;
  };
}
