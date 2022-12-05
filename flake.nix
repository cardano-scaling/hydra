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
    , ...
    } @ inputs:
    let
      system = "x86_64-linux";
      # nixpkgs enhanced with haskell.nix and crypto libs as used by iohk
      pkgs = import inputs.nixpkgs {
        inherit system;
        # REVIEW: do we need this? inherit (inputs.haskellNix) config;
        overlays = [
          inputs.haskellNix.overlay
          inputs.iohk-nix.overlays.crypto
        ];
      };
      hydraProject =
        pkgs.haskell-nix.project' {
          src = inputs.self.outPath;
          projectFileName = "cabal.project";
          compiler-nix-name = "ghc8107";
          # This is used by `nix develop .` to open a shell for use with
          # `cabal`, `hlint` and `haskell-language-server`
          shell.tools = {
            cabal = { };
            hlint = { };
            haskell-language-server = { };
          };
          # Non-Haskell shell tools go here
          shell.buildInputs = with pkgs; [
            nixpkgs-fmt
          ];
          inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP; };
          modules = [
            # Set libsodium-vrf on cardano-crypto-{praos,class}. Otherwise they depend
            # on libsodium, which lacks the vrf functionality.
            ({ pkgs, lib, ... }:
              # Override libsodium with local 'pkgs' to make sure it's using
              # overriden 'pkgs', e.g. musl64 packages
              {
                packages.cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
                packages.cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              }
            )
          ];
        };
    in
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
      {
        inherit hydraProject;
        # hydraJobs = std.harvest self [ "automation" "hydraJobs" ];
      };

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = true;
  };
}
