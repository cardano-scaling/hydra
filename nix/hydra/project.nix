{ self, ... }: {
  perSystem = { compiler, inputMap, pkgs, ... }:
    let
      hsPkgs = pkgs.haskell-nix.project {
        src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
          name = "hydra";
          src = self;
          filter = path: _type:
            # Blacklist of paths which do not affect the haskell build. The smaller
            # the resulting list of files is, the less likely we have redundant
            # rebuilds.
            builtins.all (x: baseNameOf path != x) [
              "flake.nix"
              "flake.lock"
              "nix"
              ".github"
              "demo"
              "docs"
              "sample-node-config"
              "spec"
              "testnets"
            ];
        };
        projectFileName = "cabal.project";

        inherit inputMap;

        compiler-nix-name = compiler;

        modules = [
          # Strip debugging symbols from exes (smaller closures)
          {
            packages = {
              hydra-cardano-api.writeHieFiles = true;
              hydra-chain-observer.writeHieFiles = true;
              hydra-cluster.writeHieFiles = true;
              hydra-node.writeHieFiles = true;
              visualize-logs.writeHieFiles = true;
              hydra-plutus.writeHieFiles = true;
              hydra-plutus-extras.writeHieFiles = true;
              hydra-prelude.writeHieFiles = true;
              hydra-test-utils.writeHieFiles = true;
              hydra-tx.writeHieFiles = true;
              hydra-tui.writeHieFiles = true;
              hydraw.writeHieFiles = true;
              hydra-node.dontStrip = false;
              visualize-logs.dontStrip = false;
              hydra-tui.dontStrip = false;
              hydraw.dontStrip = false;
            };
          }
          # Use different static libs on darwin
          # TODO: Always use these?
          (pkgs.lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
            packages.hydra-node.ghcOptions = with pkgs; [
              "-L${lib.getLib static-gmp}/lib"
              "-L${lib.getLib static-libsodium-vrf}/lib"
              "-L${lib.getLib static-secp256k1}/lib"
              "-L${lib.getLib static-openssl}/lib"
              "-L${lib.getLib static-libblst}/lib"
              "-L${lib.getLib static-snappy}/lib"
            ];
          })
          # Always use static snappy (from overlay, see nix/static-libs.nix)
          {
            # XXX: Instead of patching to the static-snappy here for all binaries, we
            # could try have both static and dynamic libs in the pkgs.snappy
            # derivation? Using only the static libs in pkgs.snappy results in
            # libHSsnappy relocation / symbol not found errors.
            packages = {
              hydra-node.ghcOptions = [ "-L${pkgs.lib.getLib pkgs.static-snappy}/lib" ];
              hydra-tui.ghcOptions = [ "-L${pkgs.lib.getLib pkgs.static-snappy}/lib" ];
              hydra-chain-observer.ghcOptions = [ "-L${pkgs.lib.getLib pkgs.static-snappy}/lib" ];
              visualize-logs.ghcOptions = [ "-L${pkgs.lib.getLib pkgs.static-snappy}/lib" ];
              hydraw.ghcOptions = [ "-L${pkgs.lib.getLib pkgs.static-snappy}/lib" ];
            };
          }
          {
            # lib:ghc is a bit annoying in that it comes with it's own build-type:Custom, and then tries
            # to call out to all kinds of silly tools that GHC doesn't really provide.
            # For this reason, we try to get away without re-installing lib:ghc for now.
            reinstallableLibGhc = false;
          }
          # XXX: Fix missing dependency onto protobuf in the haskell.nix derivation
          {
            packages.proto-lens-protobuf-types.components.library.build-tools = [ pkgs.protobuf ];
            packages.proto-lens-etcd.components.library.build-tools = [ pkgs.protobuf ];
          }
          # Add etcd as build dependency of hydra-node (template haskell embedding not tracked by cabal)
          {
            packages.hydra-node.components.library.build-tools = [ pkgs.etcd_3_5 ];
          }
          # Add static sqlite as pkgconfig dependency for direct-sqlite (used by sqlite-simple).
          # Using the static variant ensures libsqlite3 is linked into the binary so that
          # Docker images do not need to ship the shared library at runtime.
          {
            packages.hydra-node.components.library.pkgconfig = [ [ pkgs.static-sqlite ] ];
          }
          # amazonka-* 2.0 packages export duplicate record fields without
          # enabling the DuplicateRecordFields extension; harmless in GHC 9.6
          # but an error in GHC 9.12+. Enable the extension per-package.
          # We also skip their haddocks: the same error fires under haddock
          # and haskell.nix has no per-package option to inject extensions
          # there. They are dependencies, not a target for hydra docs.
          {
            packages.amazonka.ghcOptions = [ "-XDuplicateRecordFields" ];
            packages.amazonka.doHaddock = false;
            packages.amazonka-core.ghcOptions = [ "-XDuplicateRecordFields" ];
            packages.amazonka-core.doHaddock = false;
            packages.amazonka-s3.ghcOptions = [ "-XDuplicateRecordFields" ];
            packages.amazonka-s3.doHaddock = false;
            packages.amazonka-sso.ghcOptions = [ "-XDuplicateRecordFields" ];
            packages.amazonka-sso.doHaddock = false;
            packages.amazonka-sts.ghcOptions = [ "-XDuplicateRecordFields" ];
            packages.amazonka-sts.doHaddock = false;
          }
        ];
      };

    in
    {
      _module.args = { inherit hsPkgs; };
    };
}
