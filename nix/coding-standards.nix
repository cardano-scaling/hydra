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

      # The MAlonzo-extracted Haskell under hydra-agda/generated is machine-generated
      # (regenerate.sh) and must not be reformatted/linted; treefmt would fight the
      # extractor. Exclude it globally (covers fourmolu, hlint, typos).
      treefmt.settings.global.excludes = [ "hydra-agda/generated/**" ];

    };
}
