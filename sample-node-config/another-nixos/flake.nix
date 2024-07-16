{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cardano-node.url = "github:IntersectMBO/cardano-node/8.9.3";
    hydra.url = "github:cardano-scaling/hydra/0.17.0";
    mithril.url = "github:input-output-hk/mithril/2423.0";
  };


  outputs =
    { self
    , nixpkgs
    , nixos-generators
    , cardano-node
    , hydra
    , mithril
    , ...
    }@inputs:
    let
      system = "x86_64-linux";
    in
    {
      packages."${system}" = {
        vb = nixos-generators.nixosGenerate {
          inherit system;
          specialArgs = {
            inherit
              system
              cardano-node
              hydra
              mithril;
            diskSize = 20 * 1024;
          };
          modules = [
            ./configuration.nix
          ];
          # format = "docker";
          # format = "virtualbox";
          format = "qcow";
        };
      };
    };


  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://hydra-node.cachix.org"
      "https://cardano-scaling.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "hydra-node.cachix.org-1:vK4mOEQDQKl9FTbq76NjOuNaRD4pZLxi1yri31HHmIw="
      "cardano-scaling.cachix.org-1:RKvHKhGs/b6CBDqzKbDk0Rv6sod2kPSXLwPzcUQg9lY="
    ];
  };
}
