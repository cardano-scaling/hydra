# Docker images built from our packages. NOTE: These images don't include any
# metadata, as this is only added by the Github workflow.

{ hydraPackages # as defined in packages.nix
, pkgs
}:
{
  hydra-node = pkgs.dockerTools.buildImage {
    name = "hydra-node";
    tag = "latest";
    created = "now";
    contents = [
      pkgs.busybox
      pkgs.etcd
    ];
    config = {
      Entrypoint = [ "${hydraPackages.hydra-node-static}/bin/hydra-node" ];
    };
  };

  hydra-node-for-netem = pkgs.dockerTools.buildImage {
    name = "hydra-node-for-netem";
    tag = "latest";
    created = "now";
    contents = [
      pkgs.iproute2
      pkgs.busybox
      pkgs.etcd
    ];
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
      WorkingDir = "/static";
    };
    contents = [
      (pkgs.runCommand "hydraw-static-files" { } ''
        mkdir $out
        ln -s ${../../hydraw/static} $out/static
      '')
    ];
  };

  hydra-chain-observer = pkgs.dockerTools.buildImage {
    name = "hydra-chain-observer";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydra-chain-observer-static}/bin/hydra-chain-observer" ];
    };
  };
}
