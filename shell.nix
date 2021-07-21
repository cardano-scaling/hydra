# A shell setup providing build tools and utilities for a development
# environment. This is now based on haskell.nix and it's haskell-nix.project
# (see 'default.nix').
{ compiler ? "ghc8104"
  # nixpkgs 21.05 at 2021-07-19
, pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/4181644d09b96af0f92c2f025d3463f9d19c7790.tar.gz") { }

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
let
  libs = [
    libsodium-vrf
    pkgs.systemd
    pkgs.zlib
    pkgs.zeromq
  ];

  tools = [
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
    # For validating JSON instances against a pre-defined schema
    pkgs.python3Packages.jsonschema
    pkgs.yq
  ];

  haskellNixShell = hsPkgs.shellFor {
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

    buildInputs = libs ++ tools;

    # Disable haddocks as it's currently failing for the 'plutus-ledger' package
    withHoogle = false;
  };

  # A "cabal-only" shell which does not use haskell.nix
  cabalShell = pkgs.mkShell {
    name = "hydra-node-cabal-shell";

    buildInputs = libs ++ [
      pkgs.haskell.compiler.${compiler}
      pkgs.cabal-install
      pkgs.git
      pkgs.pkgconfig
    ];

    # Ensure that libz.so and other libraries are available to TH splices.
    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;

    # Force a UTF-8 locale because many Haskell programs and tests
    # assume this.
    LANG = "en_US.UTF-8";

    # Make the shell suitable for the stack nix integration
    # <nixpkgs/pkgs/development/haskell-modules/generic-stack-builder.nix>
    GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    STACK_IN_NIX_SHELL = "true";
  };

in
haskellNixShell // { cabalOnly = cabalShell; }
