# Docker images built from our packages. NOTE: These images don't include any
# metadata, as this is only added by the Github workflow.

{ lib, ... }: {

  perSystem = { pkgs, self', system, ... }:
    let
      # Only x86_64-linux has the musl cross build, so it is the only system
      # where a *-static package exists (see packages.nix). Everything below
      # that insists on one stays gated to it.
      hasStatic = system == "x86_64-linux";

      # The hydra-node the image runs. On aarch64-linux there is no static
      # build yet, so the image ships the natively linked binary; buildImage
      # pulls its runtime closure in along with it, which is why
      # docker-hydra-node-for-netem has always been able to do the same.
      hydraNode =
        if hasStatic
        then self'.packages.hydra-node-static
        else self'.packages.hydra-node;
    in
    lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
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
            Entrypoint = [ "${hydraNode}/bin/hydra-node" ];
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
      } // lib.optionalAttrs hasStatic {
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
