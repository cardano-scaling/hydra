# Docker images built from our packages

{ hydraPackages # as defined in packages.nix
, system ? builtins.currentSystem
, nixpkgs ? <nixpkgs>
}:
let
  pkgs = import nixpkgs { inherit system; };
in
{
  hydra-node = pkgs.dockerTools.buildImage {
    name = "hydra-node";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydra-node-static}/bin/hydra-node" ];
    };
  };

  hydra-tui = pkgs.dockerTools.buildImage {
    name = "hydra-tui";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydra-tui-static}/bin/hydra-tui" ];
    };
  };

  hydraw = pkgs.dockerTools.buildImage {
    name = "hydraw";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydraw-static}/bin/hydraw" ];
    };
  };
}
