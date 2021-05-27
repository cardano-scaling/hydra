# A very simple shell.nix file for setting up necessary build tools. This is
# likely going to be updated using the iohk-specific nixpkgs and a haskel.nix
# derivation of our cabal.project.
{ compiler ? "ghc8104"
  # Latest haskell.nix for more likely cache hits
, haskellNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/haskell.nix/archive/abb289ff961fb1bd98eb0340088c432f15109807.tar.gz")
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
}:
let
  hls = pkgs.haskell-nix.tool compiler "haskell-language-server" "latest";
  ghc = pkgs.haskell-nix.compiler.${compiler};
  cabal-install = pkgs.haskell-nix.tool compiler "cabal-install" "3.4.0.0";
  cabal-plan = pkgs.haskell-nix.tool compiler "cabal-plan" "latest";
  fourmolu = pkgs.haskell-nix.tool compiler "fourmolu" "latest";
  ghcid = pkgs.haskellPackages.ghcid;
  graphmod = pkgs.haskellPackages.graphmod;
  hspec-discover = pkgs.haskellPackages.hspec-discover;
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
in
pkgs.mkShell rec {
  name = "hydra-node-env";

  tools = [
    cabal-install
    cabal-plan
    fourmolu
    ghc
    hls
    ghcid
    graphmod
    hspec-discover
    # For discovering libs (below)
    pkgs.pkgconfig
    # Used in local-cluster
    cardanoNodePkgs.cardano-node
    cardanoNodePkgs.cardano-cli
    # Handy to interact with the hydra-node via websockets
    pkgs.ws
  ];

  libs = [
    libsodium-vrf
    pkgs.systemd
    pkgs.zlib
    pkgs.zeromq
  ];

  buildInputs = tools ++ libs;

  # Ensure that libz.so and other libraries are available to TH splices.
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;

  # Force a UTF-8 locale because many Haskell programs and tests
  # assume this.
  LANG = "en_US.UTF-8";

  # Make the shell suitable for the stack nix integration
  # <nixpkgs/pkgs/development/haskell-modules/generic-stack-builder.nix>
  GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  STACK_IN_NIX_SHELL = "true";
}
