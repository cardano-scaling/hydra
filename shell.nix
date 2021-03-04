# A very simple shell.nix file for setting up necessary build tools. This is
# likely going to be updated using the iohk-specific nixpkgs and a haskel.nix
# derivation of our cabal.project.
{ pkgs ? import <nixpkgs> { }
, compiler ? "ghc8102"
  # Import Haskell.nix master as of 2020-10-13, just for building hls.
, haskellNix ? import
    (builtins.fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/40a26afa33b421d7ede240b5d6c2a9a22313cb2b.tar.gz";
      sha256 = "1cd6i1rrxxqnrg659zcq0xhkind67q0kx1gddr9sni8cdhwdlvqb";
    })
    { }
}:

with pkgs;
let
  hls = (haskellNix.pkgs.haskell-nix.hackage-package {
    name = "haskell-language-server";
    version = "latest";
    compiler-nix-name = compiler;
  }).components.exes.haskell-language-server;

  ghc = haskell.compiler.${compiler};
in
mkShell rec {
  name = "hydra-node-env";

  tools = [
    ghc
    cabal-install
    haskellPackages.hoogle
    hls
  ];

  libs = [
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
