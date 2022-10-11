{ compiler ? "ghc8107"

, system ? builtins.currentSystem

, haskellNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/haskell.nix/archive/28dbf2f4bd32a4fbd1a2e9de45d02ad977b062d9.tar.gz")
    { }

  # nixpkgs-unstable as also used by cardano-node, cardano-ledger et al
, nixpkgsSrc ? builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/1882c6b7368fd284ad01b0a5b5601ef136321292.tar.gz"
}:
let
  pkgs = import nixpkgsSrc (haskellNix.nixpkgsArgs // {
    overlays =
      # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
      haskellNix.overlays;
  });

  libsodium-vrf = pkgs.libsodium.overrideAttrs (oldAttrs: {
    name = "libsodium-1.0.18-vrf";
    src = pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "libsodium";
      # branch tdammers/rebased-vrf
      rev = "66f017f16633f2060db25e17c170c2afa0f2a8a1";
      sha256 = "12g2wz3gyi69d87nipzqnq4xc6nky3xbmi2i2pb2hflddq8ck72f";
    };
    nativeBuildInputs = [ pkgs.autoreconfHook ];
    configureFlags = "--enable-static";
  });

  hsPkgs = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "hydra-poc";
      src = ./.;
    };
    projectFileName = "cabal.project";
    compiler-nix-name = compiler;

    modules = [
      # Allow reinstallation of terminfo, which wasn't installed with the cross compiler to begin with.
      ({ lib, ...}: { options.nonReinstallablePkgs = lib.mkOption { apply = lib.remove "terminfo"; }; })
      # set libsodium-vrf on cardano-crypto-{praos,class}. Otherwise they depend on libsodium, which lacks
      # the vrf functionality.
      ({ pkgs, lib, ... }: {
        # https://github.com/input-output-hk/cardano-wallet/commit/ced95e1b84ce8d9faa53268be45e96701ccc16e9
        packages.cardano-config.components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitMinimal ];

        # https://github.com/input-output-hk/iohk-nix/pull/488
        packages.cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ libsodium-vrf pkgs.secp256k1 ] ];
        packages.cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ libsodium-vrf ] ];
      })
    ];
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
