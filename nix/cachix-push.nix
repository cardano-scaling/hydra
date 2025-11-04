_: {
  perSystem = { pkgs, ... }: {
    apps.cachix-push = {
      type = "app";
      program = "${pkgs.writeShellApplication {
        name = "cachix-push";
        meta.description = "Build all flake outputs and push them to cachix.";
        text = ''
          ${pkgs.omnix}/bin/om ci run | ${pkgs.cachix}/bin/cachix push cardano-scaling
        '';
      }}/bin/cachix-push";
    };
  };
}
