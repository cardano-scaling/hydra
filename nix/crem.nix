# The node requires Crem API extensions absent from v0.1.1.0.
# Keep this patch in sync with the local Cabal dependency; see
# docs/head-logic/crem-dependency.md for setup and replacement instructions.
_: {
  perSystem = { pkgs, ... }: {
    _module.args.cremModule = {
      packages.crem = {
        src = pkgs.fetchFromGitHub {
          owner = "marcosh";
          repo = "crem";
          rev = "f06f8b47ac83d3e7448493bdee374c5f952f7f4c";
          hash = "sha256-MeY8Sz+b11FifqVMJ8LwgqA6unNE4P4L26tkv98CAQY=";
        };
        # The patch adds exposed modules to the upstream package description.
        package-description-override = pkgs.lib.mkForce null;
        patches = [ ./crem-api.patch ];
      };
    };
  };
}
