# Build checks for our local packages. Because the packages are built with
# -Werror by default (see project.nix), these point at the *same* derivations as
# `.#packages` / `.#devShells.*-tests` and the builds GitHub CI pushes to the
# cardano-scaling cachix: no override, no duplicate compilation. This replaces
# the werrorwolf-generated `-werror` checks (disabled in coding-standards.nix).
#
# We gate library + sublibrary + test components only; exes and benchmarks are
# already built/verified by `nix build .#release` and the benchmark jobs.
_: {
  perSystem = { hsPkgs, lib, localHaskellPackageNames, ... }:
    let
      checkComponents = x:
        [ x.components.library ]
        ++ builtins.attrValues (x.components.sublibs or { })
        ++ builtins.attrValues (x.components.tests or { });
      comps = lib.concatMap (n: checkComponents hsPkgs.${n}) localHaskellPackageNames;
    in
    {
      checks = lib.listToAttrs (map (c: lib.nameValuePair c.name c) comps);
    };
}
