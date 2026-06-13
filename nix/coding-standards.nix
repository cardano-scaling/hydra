_: {

  perSystem = { pkgs, hsPkgs, lib, localHaskellPackageNames, ... }:
    let
      allComponents = x:
        [ x.components.library ]
        ++ lib.concatMap
          (y: builtins.attrValues x.components."${y}")
          [ "benchmarks" "exes" "sublibs" "tests" ];

    in
    {


      coding.standards.hydra = {
        enable = true;
        haskellPackages = builtins.concatMap allComponents
          (map (n: hsPkgs.${n}) localHaskellPackageNames);
        inherit (pkgs) weeder;
        haskellType = "haskell.nix";
      };

      # We build our packages with -Werror by default (see project.nix) and
      # expose lib+test build checks directly (see werror-checks.nix), so the
      # werrorwolf -Werror variants would just duplicate those derivations under
      # different store paths. Disable it; coding.standards keeps treefmt,
      # weeder and no-srp.
      werrorwolf.enable = lib.mkForce false;

    };
}
