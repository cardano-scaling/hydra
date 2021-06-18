# A shell setup providing build tools and utilities for a development
# environment. This is now based on haskell.nix and it's haskell-nix.project
# (see 'default.nix').
{ compiler ? "ghc8104"
  # Latest haskell.nix for more likely cache hits
, haskellNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/haskell.nix/archive/532e71470b41fc0fd0d3d858ea98d7f07f37d309.tar.gz")
    { }
  # Use same pkgs as haskell.nix for more likely cache hits
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgsSrc nixpkgsArgs
  # Use cardano-node master for more likely cache hits
, cardanoNodePkgs ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/cardano-node/archive/8fe46140a52810b6ca456be01d652ca08fe730bf.tar.gz")
    { gitrev = "8fe46140a52810b6ca456be01d652ca08fe730bf"; }

, hsPkgs ? import ./default.nix { }

, libsodium-vrf ? pkgs.libsodium.overrideAttrs (oldAttrs: {
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
  })
}:
hsPkgs.shellFor {
  packages = ps: with ps; [
    hydra-prelude
    hydra-node
    hydra-plutus
    local-cluster
  ];

  # Haskell.nix managed tools (via hackage)
  tools = {
    cabal = "3.4.0.0";
    fourmolu = "latest";
    haskell-language-server = "latest";
  };

  # REVIEW(SN): Libs and pkgconfig still required with haskell.nix?
  buildInputs = [
    # Libraries
    libsodium-vrf
    pkgs.systemd
    pkgs.zlib
    pkgs.zeromq
    # Tools
    pkgs.pkgconfig
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hspec-discover
    pkgs.haskellPackages.graphmod
    pkgs.haskellPackages.cabal-plan
    # Handy to interact with the hydra-node via websockets
    pkgs.ws
    # Used in local-cluster
    cardanoNodePkgs.cardano-node
    cardanoNodePkgs.cardano-cli
  ];

  # Disable haddocks as it's currently failing for the 'plutus-ledger' package
  withHoogle = false;
}
