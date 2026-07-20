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

    };
}
