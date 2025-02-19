{ compiler-nix-name

, inputMap

, pkgs

}:

pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
    name = "hydra";
    src = ./../..;
    filter = path: type:
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

  inherit inputMap compiler-nix-name;

  modules = [
    # Strip debugging symbols from exes (smaller closures)
    {
      packages.hydra-cardano-api.writeHieFiles = true;
      packages.hydra-chain-observer.writeHieFiles = true;
      packages.hydra-cluster.writeHieFiles = true;
      packages.hydra-node.writeHieFiles = true;
      packages.hydra-plutus.writeHieFiles = true;
      packages.hydra-plutus-extras.writeHieFiles = true;
      packages.hydra-prelude.writeHieFiles = true;
      packages.hydra-test-utils.writeHieFiles = true;
      packages.hydra-tx.writeHieFiles = true;
      packages.hydra-tui.writeHieFiles = true;
      packages.hydraw.writeHieFiles = true;
      packages.hydra-node.dontStrip = false;
      packages.hydra-tui.dontStrip = false;
      packages.hydraw.dontStrip = false;
    }
    # Use different static libs on darwin
    # TODO: Always use these?
    (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
      packages.hydra-node.ghcOptions = with pkgs; [
        "-L${lib.getLib static-gmp}/lib"
        "-L${lib.getLib static-libsodium-vrf}/lib"
        "-L${lib.getLib static-secp256k1}/lib"
        "-L${lib.getLib static-openssl}/lib"
        "-L${lib.getLib static-libblst}/lib"
      ];
    })
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
  ];
}
