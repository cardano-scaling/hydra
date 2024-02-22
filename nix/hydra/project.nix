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
      reinstallableLibGhc = false;
    }
  ];
}
