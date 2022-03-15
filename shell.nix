# A shell setup providing build tools and utilities for a development
# environment. This is now based on haskell.nix and it's haskell-nix.project
# (see 'default.nix').
{ compiler ? "ghc8107"
  # nixpkgs 21.11
, pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/a7ecde854aee5c4c7cd6177f54a99d2c1ff28a31.tar.gz") { }

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
    pkgs.zlib
    pkgs.lzma
  ]
  ++
  pkgs.lib.optionals (pkgs.stdenv.isLinux) [ pkgs.systemd ];

  tools = [
    pkgs.pkgconfig
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hspec-discover
    pkgs.haskellPackages.graphmod
    pkgs.haskellPackages.cabal-plan
    pkgs.haskellPackages.cabal-fmt
    # Handy to interact with the hydra-node via websockets
    pkgs.ws
    # For validating JSON instances against a pre-defined schema
    pkgs.python3Packages.jsonschema
    pkgs.yq
    # For plotting results of hydra-cluster benchmarks
    pkgs.gnuplot
    # For docs/
    pkgs.yarn
    # For pixel-painting/
    pkgs.simple-http-server
  ];

  haskellNixShell = hsPkgs.shellFor {
    # NOTE: Explicit list of local packages as hoogle would not work otherwise.
    # Make sure these are consistent with the packages in cabal.project.
    packages = ps: with ps; [
      hydra-cluster
      hydra-node
      hydra-plutus
      hydra-prelude
      hydra-test-utils
      hydra-tui
      hydra-cardano-api
      merkle-patricia-tree
      plutus-cbor
      plutus-merkle-tree
    ];

    # Haskell.nix managed tools (via hackage)
    tools = {
      cabal = "3.4.0.0";
      fourmolu = "0.4.0.0"; # 0.5.0.0 requires Cabal 3.6
      haskell-language-server = "latest";
    };

    buildInputs = libs ++ tools;

    withHoogle = true;

    # Always create missing golden files
    CREATE_MISSING_GOLDEN = 1;
  };

  # A "cabal-only" shell which does not use haskell.nix
  cabalShell = pkgs.mkShell {
    name = "hydra-node-cabal-shell";

    buildInputs = libs ++ [
      pkgs.haskell.compiler.${compiler}
      pkgs.cabal-install
      pkgs.git
      pkgs.pkgconfig
    ] ++ tools;

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
