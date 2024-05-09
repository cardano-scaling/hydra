# Docker images built from our packages. NOTE: These images don't include any
# metadata, as this is only added by the Github workflow.

{ hydraPackages # as defined in packages.nix
, system
, pkgs
}:
let
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

  hydra-explorer = pkgs.dockerTools.buildImage {
    name = "hydra-explorer";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydra-explorer-static}/bin/hydra-explorer" ];
      WorkingDir = "/";
    };
    # Copy the static files to the /static in the docker image
    copyToRoot = [
      (pkgs.runCommand "hydra-explorer-static-files" { } ''
        mkdir $out
        ln -s ${hydraPackages.hydra-explorer-web} $out/static
      '')
    ];
  };

  hydraw = pkgs.dockerTools.buildImage {
    name = "hydraw";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydraw-static}/bin/hydraw" ];
      WorkingDir = "/static";
    };
    copyToRoot = [
      (pkgs.runCommand "hydraw-static-files" { } ''
        mkdir $out
        ln -s ${../../hydraw/static} $out/static
      '')
    ];
  };
}
