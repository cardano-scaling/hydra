# Docker images built from our packages

{ hydraPackages # as defined in packages.nix
, system ? builtins.currentSystem
, nixpkgs ? <nixpkgs>
}:
let
  pkgs = import nixpkgs { inherit system; };

  alpineFromDockerHub = pkgs.dockerTools.pullImage {
    imageName = "alpine";
    imageDigest = "sha256:8914eb54f968791faf6a8638949e480fef81e697984fba772b3976835194c6d4";
    sha256 = "0mn4hr0cpwa8g45djnivmky3drdvsb38r65hlbx9l88i5p8qhld6";
  };
in
{
  hydra-node = pkgs.dockerTools.buildImage {
    name = "hydra-node";
    tag = "latest";
    created = "now";
    fromImage = alpineFromDockerHub;
    copyToRoot = pkgs.buildEnv {
      name = "hydra-node-bin";
      paths = [ hydraPackages.hydra-node-static ];
      pathsToLink = [ "/bin" ];
    };
    config = {
      Entrypoint = [ "/bin/hydra-node" ];
    };
  };
}
