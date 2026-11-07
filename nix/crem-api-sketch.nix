# Reproduce the local Crem experiment in sandboxed builds. Cabal development
# uses cabal.project.local; Nix uses the matching patch recorded in the review.
_: {
  perSystem = { pkgs, ... }: {
    _module.args.cremApiSketchModule = {
      packages.crem = {
        src = pkgs.fetchFromGitHub {
          owner = "marcosh";
          repo = "crem";
          rev = "f06f8b47ac83d3e7448493bdee374c5f952f7f4c";
          hash = "sha256-MeY8Sz+b11FifqVMJ8LwgqA6unNE4P4L26tkv98CAQY=";
        };
        # The sketch adds exposed modules to the upstream package description.
        package-description-override = pkgs.lib.mkForce null;
        patches = [ ../docs/head-logic/crem-api-sketch.patch ];
      };
    };
  };
}
