{ inputs, ... }: {

  imports = [
    inputs.flake-root.flakeModule
    inputs.mission-control.flakeModule
  ];

  perSystem = {
    mission-control.scripts = {
      werror = {
        description = "Run werror checks";
        exec = ''
          cabal build all --ghc-options="-Werror"
        '';
        category = "Checks";
      };
    };
  };
}
