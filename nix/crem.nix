# Allow the pinned Crem fork while retaining the check for other source dependencies.
{ self, inputs, ... }: {
  perSystem = { pkgs, lib, system, ... }:
    let
      project = builtins.readFile ../cabal.project;
      cremSource = ''
        source-repository-package
          type: git
          location: https://github.com/cardano-scaling/crem
          tag: ${inputs.crem.rev}
      '';
    in
    {
      checks.no-srp = lib.mkForce (
        assert lib.assertMsg (lib.hasInfix cremSource project)
          "cabal.project must pin Crem to the revision in flake.lock";
        inputs.hydra-coding-standards.inputs.lint-utils.linters.${system}.no-srp {
          src = self;
          cabal-project-file = pkgs.writeText "cabal.project-without-crem"
            (builtins.replaceStrings [ cremSource ] [ "" ] project);
        }
      );
    };
}
