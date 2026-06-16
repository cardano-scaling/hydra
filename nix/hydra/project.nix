{ self, ... }: {
  perSystem = { compiler, inputMap, pkgs, localHaskellPackageNames, ... }:
    let
      mkProject = extraModules: pkgs.haskell-nix.project {
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
          # GHC 9.6.7 has a haddock bug (tyConStupidTheta) that panics on data
          # types declared with the deprecated DatatypeContexts extension.
          # Skip haddocks for the affected upstream packages so `withHoogle`
          # can still index everything else. hydra-cardano-api re-exports
          # Cardano.Api broadly (module X / module Cardano.Api), which surfaces
          # the same DatatypeContexts types and panics haddock on our wrapper
          # package too.
          {
            packages.cardano-diffusion.doHaddock = false;
            packages.cardano-ledger-shelley.doHaddock = false;
            packages.ouroboros-network.doHaddock = false;
            packages.hydra-cardano-api.doHaddock = false;
          }
        ] ++ extraModules;
      };

      # Native project, built with -Werror for our own packages. This is the
      # single warning gate for nix builds: `.#packages`, `.#devShells.*-tests`
      # and `.#checks` all share these derivations, so there is no separate
      # -Werror rebuild (cf. the old werrorwolf checks). Scoped per-package so
      # upstream dependencies keep coming from the binary cache. `cabal
      # build`/repl in the dev shell ignore these ghcOptions, so day-to-day
      # iteration stays lenient; `just lint` is the cabal-side -Werror gate.
      hsPkgs = mkProject [
        {
          packages = pkgs.lib.genAttrs localHaskellPackageNames (_: {
            ghcOptions = [ "-Werror" ];
          });
        }
      ];

      # Base project, no -Werror. Consumed only by packages.nix for the
      # musl/static cross build, so the static release stays lenient, matching
      # the previous werrorwolf scope. Dependency derivations are identical to
      # hsPkgs (the -Werror module only touches our own packages), so this does
      # not cause duplicate dependency builds.
      hsPkgsBase = mkProject [ ];

    in
    {
      _module.args = { inherit hsPkgs hsPkgsBase; };
    };
}
