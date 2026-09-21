# What each test-suite needs at *run* time, in one place.
#
# Consumed by:
#   nix/hydra/test-checks.nix   -> the cached `checks.test-<pkg>` runs
#   nix/hydra/packages.nix      -> the matching `<pkg>-tests` dev shells
#   nix/hydra/werror-checks.nix -> to drop a test *build* check where a run
#                                  check already covers it
#
# `tools` and `env` are shared verbatim by the shell and the check so the two
# cannot drift. `fixtures` is check-only: the dev shell already runs inside the
# real worktree, where those files are on disk.
#
# hydra-cluster and hydra-tui are deliberately absent: the first spawns real
# cardano-node devnets and the second needs a pty, so both keep their
# hand-written shells and their `nix develop` CI step.
_: {
  perSystem = { pkgs, lib, system, testSuitePackageNames, ... }:
    let
      repoRoot = ../..;

      # Files a suite reads at run time that haskell.nix does not put into the
      # component source root. clean-cabal-component.nix keeps only the cabal
      # file, licences, hs-source-dirs, include-dirs, data-files and
      # extra-source-files, which leaves out every `golden/` tree, the aiken
      # project and the demo configs. Paths are repo-relative and are restored
      # at their repo-relative position, because that is how the tests address
      # them (hydra-node's ConfigSpec reads ../demo/configs/alice.yaml).
      #
      # Materialised as its own store path so that updating a fixture
      # invalidates the *run* and not the compile; declaring them as
      # `extraSrcFiles` instead would rebuild the test binary on every golden
      # change.
      fixtureSrc = paths: lib.fileset.toSource {
        root = repoRoot;
        fileset = lib.fileset.unions (map (p: repoRoot + "/${p}") paths);
      };

      # Aiken resolves dependencies from the network unless they are already
      # in its XDG cache. Pre-build the cache with the pinned stdlib so the
      # blueprint test cannot fail on registry outages; GoldenSpec points
      # aiken at it via HYDRA_AIKEN_CACHE.
      #
      # The version is read from aiken.toml so it cannot drift silently: a
      # stdlib bump there makes this fetch fail on the stale hash (update
      # the hash below alongside).
      aikenCache =
        let
          aikenToml = builtins.replaceStrings [ "\n" ] [ " " ]
            (builtins.readFile ../../hydra-plutus/aiken.toml);
          stdlibVersion = builtins.head
            (builtins.match ''.*name = "aiken-lang/stdlib" *version = "([^"]+)".*'' aikenToml);
          aikenStdlib = pkgs.fetchzip {
            url = "https://github.com/aiken-lang/stdlib/archive/refs/tags/${stdlibVersion}.zip";
            hash = "sha256-PfnRpyt+8WAqC5No4RADag/UcFVjZhV1CtEgT8sPPKA=";
          };
        in
        pkgs.runCommand "aiken-cache" { nativeBuildInputs = [ pkgs.zip ]; } ''
          mkdir -p $out/aiken/packages work/stdlib-${stdlibVersion}
          cp -r ${aikenStdlib}/. work/stdlib-${stdlibVersion}/
          # zip records the permission bits and store files are read-only;
          # extracting read-only directories into build/packages then breaks
          # the very next mkdir inside them.
          chmod -R u+w work
          (cd work && zip -q -r -X $out/aiken/packages/aiken-lang-stdlib-${stdlibVersion}.zip stdlib-${stdlibVersion})
        '';

      defaults = {
        # Put on PATH for both the check and the dev shell.
        tools = [ ];
        # Exported in the check and set in the dev shell.
        env = { };
        # Exported in the check only, where the dev shell must not inherit it.
        checkEnv = { };
        # Repo-relative paths restored around the check's working directory.
        fixtures = [ ];
        # null means every system in nix/systems.nix.
        systems = null;
      };

      raw = {
        hydra-agda = { };

        hydra-plutus-extras = { };

        hydra-tx = { fixtures = [ "hydra-tx/golden" ]; };

        hydra-chain-observer = { fixtures = [ "hydra-chain-observer/golden" ]; };

        hydra-plutus = {
          tools = [ pkgs.aiken ];
          env.HYDRA_AIKEN_CACHE = aikenCache;
          # GoldenSpec shells out to `aiken build -t compact` in the package
          # directory, and goldenScript reads scripts/<name>.plutus.
          fixtures = [
            "hydra-plutus/aiken.toml"
            "hydra-plutus/aiken.lock"
            "hydra-plutus/validators"
            "hydra-plutus/scripts"
          ];
        };

        hydra-node = {
          tools = [
            pkgs.check-jsonschema
            pkgs.etcd # for the etcd servers and the etcdctl command
          ];
          # Parity with .github/workflows/ci-nix.yaml: with CI set, `onlyLocal`
          # skips the ~10 minute etcd recycle test. CI_NIGHTLY stays unset, so
          # `onlyNightly` (S3, Mithril, known networks, deep fuzz) stays
          # pending. Check-only, so `nix develop .#hydra-node-tests` and
          # `just stress-test` keep running the local-only test.
          checkEnv = {
            CI = "1";
            # etcd's darwin fsync is F_FULLFSYNC, a full device flush per WAL
            # append (client/pkg/fileutil/sync_darwin.go), which throttles the
            # aarch64-darwin builder to ~10 puts/s: "resends messages" pushes
            # 1000 sequential puts and timed out at ~revision 600 of its 60s
            # budget. Durability buys nothing for a sandbox tmpdir that is
            # deleted after the run, so turn it off here. Check-only: local
            # etcd keeps real fsync semantics.
            ETCD_UNSAFE_NO_FSYNC = "true";
          };
          fixtures = [
            "hydra-node/golden"
            # ConfigSpec loads ../demo/configs/*.yaml, which lives outside the
            # package and is excluded from the haskell.nix project source.
            "demo/configs"
          ];
        };
      };
    in
    {
      # The assertion lives inside the value, not in front of the attrset: a
      # module that both consumes and provides a `_module.args` key must not
      # force the one it consumes while constructing the one it provides.
      _module.args.hydraTestRuntime =
        assert lib.assertMsg
          (lib.all (n: lib.elem n testSuitePackageNames) (lib.attrNames raw))
          "nix/hydra/test-runtime.nix names a package with no test-suite";
        lib.mapAttrs
          (_: cfg:
            let c = defaults // cfg; in
            c // {
              enabled = c.systems == null || lib.elem system c.systems;
              fixtures =
                if c.fixtures == [ ]
                then null
                else { paths = c.fixtures; src = fixtureSrc c.fixtures; };
            })
          raw;
    };
}
