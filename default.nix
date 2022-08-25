{ compiler ? "ghc8107"

, system ? builtins.currentSystem

, haskellNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/haskell.nix/archive/a500d951fbf198c9ec0710b7dc9d16328341fbfa.tar.gz")
    { }

, iohkNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/iohk-nix/archive/9a604d01bd4420ab7f396f14d1947fbe2ce7db8b.tar.gz")
    { }

, nixpkgsSrc ? haskellNix.sources.nixpkgs-unstable
}:
let
  pkgs = import nixpkgsSrc (haskellNix.nixpkgsArgs // {
    overlays =
      # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
      haskellNix.overlays
        # needed for cardano-crypto-class which uses a patched libsodium
        ++ iohkNix.overlays.crypto;
  });

  hsPkgs = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "hydra-poc";
      src = ./.;
    };
    projectFileName = "cabal.project";
    compiler-nix-name = compiler;

    modules = [{
      # https://github.com/input-output-hk/cardano-wallet/commit/ced95e1b84ce8d9faa53268be45e96701ccc16e9
      packages.cardano-config.components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitMinimal ];

      # https://github.com/input-output-hk/iohk-nix/pull/488
      packages.cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
      packages.cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
    }];
  };

  # Add cardano-node & cardano-cli for our shell environments.
  # This is stable as it doesn't mix dependencies with this code-base; the
  # fetched binaries are the "standard" builds that people test. This should be
  # fast as it mostly fetches Hydra (CI) caches without building much.
  cardano-node = import
    (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "1.35.3";
      sha256 = "020fwimsm24yblr1fmnwx240wj8r3x715p89cpjgnnd8axwf32p0";
    })
    { };
in
{
  inherit compiler pkgs hsPkgs cardano-node;
}
