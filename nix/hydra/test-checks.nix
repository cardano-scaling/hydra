# Run the cabal test-suites as flake checks, so what nix caches is a *passing
# run* rather than just a compiled binary: an unchanged package skips its suite
# entirely, on `just check`, on selfci and in GitHub CI.
#
# The test binaries themselves are untouched. haskell.nix already derives
# `hsPkgs.<pkg>.checks.tests` (lib/check.nix), which only unpacks the component
# source, runs the binary and tees stdout; and preCheck/postCheck/testFlags are
# read by that derivation alone, never by the component builder. So everything
# wired up here stays out of the builds GitHub CI pushes to the cardano-scaling
# cachix.
#
# Per-suite requirements live in nix/hydra/test-runtime.nix.
_: {
  perSystem = { lib, hsPkgs, hydraTestRuntime, ... }:
    let
      mkCheck = name: cfg:
        let
          exports = lib.concatStringsSep "\n"
            (lib.mapAttrsToList (k: v: "export ${k}=${lib.escapeShellArg "${v}"}")
              (cfg.env // cfg.checkEnv));
          fixtureArgs = lib.escapeShellArgs (cfg.fixtures.paths or [ ]);
        in
        hsPkgs.${name}.checks.tests.overrideAttrs (old: {
          # check.nix is `mkdir $out; runHook preCheck; ... | tee
          # $out/test-stdout; runHook postCheck`, after a patchPhase that has
          # already cd'd into the package directory. So here $out exists and
          # the working directory is that package directory, inside the
          # unpacked source made writable by unpackPhase - which is also what
          # lets hydra-plutus' GoldenSpec rewrite plutus.json and create
          # build/.
          preCheck = (old.preCheck or "") + ''
            export HOME=$TMPDIR
            export XDG_CONFIG_HOME=$TMPDIR/xdg-config
            export XDG_CACHE_HOME=$TMPDIR/xdg-cache
            mkdir -p "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME"
            export PATH=${lib.makeBinPath cfg.tools}''${PATH:+:$PATH}
            ${exports}
          '' + lib.optionalString (cfg.fixtures != null) ''
            # Fixture paths are repo-relative and the working directory is the
            # package directory, so they go back one level up, merging into the
            # unpacked component source.
            cp -r ${cfg.fixtures.src}/. ..
            ( cd .. && chmod -R u+w ${fixtureArgs} )
          '';

          postCheck = (old.postCheck or "") + ''
            cp junit.xml $out/junit.xml
          '' + lib.optionalString (cfg.fixtures != null) ''
            # Guard against a vacuous pass. goldenCBOR
            # (hydra-node/testlib/Test/Hydra/CBOR.hs) *creates* a missing
            # golden file and then reports success, so a suite whose golden
            # tree failed to arrive would be green while asserting nothing.
            # (hspec-golden-aeson only does that with CREATE_MISSING_GOLDEN
            # set, which is a dev-shell convenience and not set here.) A run
            # with all fixtures present never writes to them, so any
            # difference means exactly that.
            for f in ${fixtureArgs}; do
              if ! diff -r -q "${cfg.fixtures.src}/$f" "../$f"; then
                echo "FAIL: the test run created or modified fixtures under $f," >&2
                echo "      so golden assertions in this suite proved nothing." >&2
                exit 1
              fi
            done
          '';
        });
    in
    {
      # `test-<pkg>` rather than `<pkg>-tests`, because `packages.<pkg>-tests`
      # (the dev shell) already exists and nix/ci.nix puts packages and checks
      # into the same hydraJobs tree.
      checks = lib.mapAttrs' (n: c: lib.nameValuePair "test-${n}" (mkCheck n c))
        (lib.filterAttrs (_: c: c.enabled) hydraTestRuntime);
    };
}
