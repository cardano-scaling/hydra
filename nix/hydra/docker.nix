# Docker images built from our packages. NOTE: These images don't include any
# metadata, as this is only added by the Github workflow.

{ lib, ... }: {

  perSystem = { pkgs, self', ... }:
    lib.mkIf pkgs.stdenv.isLinux {
      packages = {
        docker-hydra-node = pkgs.dockerTools.buildImage {
          name = "hydra-node";
          tag = "latest";
          created = "now";
          copyToRoot = pkgs.buildEnv {
            name = "hydra-node-env";
            paths = [
              pkgs.busybox
              pkgs.dockerTools.caCertificates
            ];
          };
          config = {
            Entrypoint = [ "${self'.packages.hydra-node-static}/bin/hydra-node" ];
          };
        };

        docker-hydra-node-for-netem = pkgs.dockerTools.buildImage {
          name = "hydra-node-for-netem";
          tag = "latest";
          created = "now";
          copyToRoot = pkgs.buildEnv {
            name = "hydra-node-for-netem-env";
            paths = [
              pkgs.busybox
              pkgs.iproute2
            ];
          };
          config = {
            Entrypoint = [ "${self'.packages.hydra-node}/bin/hydra-node" ];
          };
        };

        docker-hydra-tui = pkgs.dockerTools.buildImage {
          name = "hydra-tui";
          tag = "latest";
          created = "now";
          config = {
            Entrypoint = [ "${self'.packages.hydra-tui-static}/bin/hydra-tui" ];
          };
        };

        docker-hydraw = pkgs.dockerTools.buildImage {
          name = "hydraw";
          tag = "latest";
          created = "now";
          config = {
            Entrypoint = [ "${self'.packages.hydraw-static}/bin/hydraw" ];
            WorkingDir = "/static";
          };
          copyToRoot = pkgs.buildEnv {
            name = "hydraw-env";
            paths = [
              (pkgs.runCommand "hydraw-static-files" { } ''
                mkdir $out
                ln -s ${../../hydraw/static} $out/static
              '')
            ];
          };
        };

        docker-hydra-chain-observer = pkgs.dockerTools.buildImage {
          name = "hydra-chain-observer";
          tag = "latest";
          created = "now";
          config = {
            Entrypoint = [ "${self'.packages.hydra-chain-observer-static}/bin/hydra-chain-observer" ];
          };
        };
      };
    };
}
