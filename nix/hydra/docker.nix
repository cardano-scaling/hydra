# Docker images built from our packages. NOTE: These images don't include any
# metadata, as this is only added by the Github workflow.

{ hydraPackages # as defined in packages.nix
, pkgs
}:
{
  hydra-node = pkgs.dockerTools.streamLayeredImage {
    name = "hydra-node";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydra-node}/bin/hydra-node" ];
    };
  };

  hydra-node-for-netem = pkgs.dockerTools.streamLayeredImage {
    name = "hydra-node-for-netem";
    tag = "latest";
    created = "now";
    contents = [
      pkgs.iproute2
      pkgs.busybox
    ];
    config = {
      Entrypoint = [ "${hydraPackages.hydra-node}/bin/hydra-node" ];
    };
  };

  hydra-tui = pkgs.dockerTools.streamLayeredImage {
    name = "hydra-tui";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydra-tui}/bin/hydra-tui" ];
    };
  };

  hydraw = pkgs.dockerTools.streamLayeredImage {
    name = "hydraw";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydraw}/bin/hydraw" ];
      WorkingDir = "/static";
    };
    contents = [
      (pkgs.runCommand "hydraw-files" { } ''
        mkdir $out
        ln -s ${../../hydraw/static} $out/static
      '')
    ];
  };

  hydra-chain-observer = pkgs.dockerTools.streamLayeredImage {
    name = "hydra-chain-observer";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydra-chain-observer}/bin/hydra-chain-observer" ];
    };
  };
}
