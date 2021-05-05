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
      "https://github.com/input-output-hk/cardano-node/archive/master.tar.gz")
    { gitrev = "0b0ab070e71f7f29bb35dfc595c6a69772b1d866"; }
}:
with pkgs;
let
  hls = haskell-nix.tool compiler "haskell-language-server" "latest";
  ghc = haskell-nix.compiler.${compiler};
  fourmolu = haskell-nix.tool compiler "fourmolu" "latest";
  ghcid = pkgs.haskellPackages.ghcid ;
  hspec-discover = pkgs.haskellPackages.hspec-discover ;
  libsodium-vrf = libsodium.overrideAttrs (oldAttrs: {
    name = "libsodium-1.0.18-vrf";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "libsodium";
      # branch tdammers/rebased-vrf
      rev = "66f017f16633f2060db25e17c170c2afa0f2a8a1";
      sha256 = "12g2wz3gyi69d87nipzqnq4xc6nky3xbmi2i2pb2hflddq8ck72f";
    };
    nativeBuildInputs = [ autoreconfHook ];
    configureFlags = "--enable-static";
  });
in
mkShell rec {
  name = "hydra-node-env";

  tools = [
    cabal-install
    fourmolu
    ghc
    hls
    ghcid
    hspec-discover
    # For discovering libs (below)
    pkgconfig
    # Used in local-cluster
    cardanoNodePkgs.cardano-node
    cardanoNodePkgs.cardano-cli
  ];

  libs = [
    libsodium-vrf
    systemd
    zlib
  ];

  buildInputs = tools ++ libs;

  # Ensure that libz.so and other libraries are available to TH splices.
  LD_LIBRARY_PATH = lib.makeLibraryPath libs;

  # Force a UTF-8 locale because many Haskell programs and tests
  # assume this.
  LANG = "en_US.UTF-8";

  # Make the shell suitable for the stack nix integration
  # <nixpkgs/pkgs/development/haskell-modules/generic-stack-builder.nix>
  GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  STACK_IN_NIX_SHELL = "true";
}
