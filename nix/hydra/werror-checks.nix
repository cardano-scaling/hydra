# Build checks for our local packages. Because the packages are built with
# -Werror by default (see project.nix), these point at the *same* derivations as
# `.#packages` / `.#devShells.*-tests` and the builds GitHub CI pushes to the
# cardano-scaling cachix: no override, no duplicate compilation. This replaces
# the `-werror` checks that hydra-coding-standards used to generate via werrorwolf.
#
# We gate library + sublibrary + test components only; exes and benchmarks are
# already built/verified by `nix build .#release` and the benchmark jobs.
#
# A test component is dropped here once nix/hydra/test-checks.nix runs it on
# this system: `checks.test-<pkg>` depends on the binary, so it subsumes the
# build gate. Suites without a run check (hydra-cluster, hydra-tui) keep theirs.
_: {
  perSystem = { hsPkgs, lib, localHaskellPackageNames, hydraTestRuntime, ... }:
    let
      runsHere = n: (hydraTestRuntime.${n} or { enabled = false; }).enabled;
      checkComponents = n:
        let x = hsPkgs.${n}; in
        [ x.components.library ]
        ++ builtins.attrValues (x.components.sublibs or { })
        ++ lib.optionals (!runsHere n) (builtins.attrValues (x.components.tests or { }));
      comps = lib.concatMap checkComponents localHaskellPackageNames;
    in
    {
      checks = lib.listToAttrs (map (c: lib.nameValuePair c.name c) comps);
    };
}
