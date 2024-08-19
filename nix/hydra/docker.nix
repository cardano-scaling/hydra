# Docker images built from our packages. NOTE: These images don't include any
# metadata, as this is only added by the Github workflow.

{ hydraPackages # as defined in packages.nix
, system
, nixpkgs
}:
let
  pkgs = import nixpkgs { inherit system; };
in
{
  hydra-node = pkgs.dockerTools.streamLayeredImage {
    name = "hydra-node";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydra-node-static}/bin/hydra-node" ];
    };
  };

  hydra-tui = pkgs.dockerTools.streamLayeredImage {
    name = "hydra-tui";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydra-tui-static}/bin/hydra-tui" ];
    };
  };

  hydra-explorer = pkgs.dockerTools.streamLayeredImage {
    name = "hydra-explorer";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydra-explorer-static}/bin/hydra-explorer" ];
      WorkingDir = "/";
    };
    # Copy the static files to the /static in the docker image
    contents = [
      (pkgs.runCommand "hydra-explorer-static-files" { } ''
        mkdir $out
        ln -s ${hydraPackages.hydra-explorer-web} $out/static
      '')
    ];
  };

  hydraw = pkgs.dockerTools.streamLayeredImage {
    name = "hydraw";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydraw-static}/bin/hydraw" ];
      WorkingDir = "/static";
    };
    contents = [
      (pkgs.runCommand "hydraw-static-files" { } ''
        mkdir $out
        ln -s ${../../hydraw/static} $out/static
      '')
    ];
  };
}
