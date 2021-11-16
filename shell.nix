# A shell setup providing build tools and utilities for a development
# environment. This is now based on haskell.nix and it's haskell-nix.project
# (see 'default.nix').
{ compiler ? "ghc8107"
  # nixpkgs 21.05 at 2021-11-16
, pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/01eaa66bb663412c31b5399334f118030a91f1aa.tar.gz") { }

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
    pkgs.lzma
  ];

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
    # For plotting results of local-cluster benchmarks
    pkgs.gnuplot
  ];

  haskellNixShell = hsPkgs.shellFor {
    packages = ps: with ps; [
      hydra-prelude
      hydra-node
      hydra-plutus
      local-cluster
      merkle-patricia-tree
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
