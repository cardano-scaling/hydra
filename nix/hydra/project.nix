{ compiler ? "ghc8107"

, system ? builtins.currentSystem

, haskellNix ? import
    (builtins.fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/0.0.64.tar.gz";
      sha256 = "0mply6n3gym8r7z4l7512phw667qzwfqkfl1zmiyrdjpdg7xqn1d";
    })
    { pkgs = import nixpkgs { inherit system; }; }

, iohk-nix ? import
    (builtins.fetchTarball {
      url = "https://github.com/input-output-hk/iohk-nix/archive/d31417fe8c8fbfb697b3ad4c498e17eb046874b9.tar.gz";
      sha256 = "0w562wdmdhp83dw9rabiijj5hk1f4l8p8f3bwlr7virakgbg8lf8";
    })
    { }

  # NOTE: use the 'repo' branch of CHaP which contains the index
, CHaP ? (builtins.fetchTarball {
    url = "https://github.com/input-output-hk/cardano-haskell-packages/archive/695c91a740abfeef0860056227c605abf6375edd.tar.gz";
    sha256 = "05dffxjxap6ncfs7x4lpp85cm7pvls0b10idpyshm4lqlrz5v92p";
  })

, nixpkgs ? iohk-nix.nixpkgs
}:
let
  # nixpkgs enhanced with haskell.nix and crypto libs as used by iohk
  pkgs = import nixpkgs {
    inherit system;
    overlays = [
      haskellNix.overlay
      iohk-nix.overlays.crypto
    ];
  };

  hsPkgs = pkgs.haskell-nix.project {
    # TODO: probably should use flake.nix inputs.self here
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "hydra";
      src = ./../..;
    };
    projectFileName = "cabal.project";
    compiler-nix-name = compiler;

    inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };

    modules = [
      # Strip debugging symbols from exes (smaller closures)
      {
        packages.hydra-node.dontStrip = false;
        packages.hydra-tui.dontStrip = false;
        packages.hydraw.dontStrip = false;
      }
      # Set libsodium-vrf on cardano-crypto-{praos,class}. Otherwise they depend
      # on libsodium, which lacks the vrf functionality.
      ({ pkgs, lib, ... }:
        # Override libsodium with local 'pkgs' to make sure it's using
        # overriden 'pkgs', e.g. musl64 packages
        {
          packages.cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
          packages.cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
        }
      )
    ];
  };
in
{
  inherit compiler pkgs hsPkgs;
}
