{ compiler ? "ghc8107"

, system ? builtins.currentSystem

, haskellNix ? import
    (builtins.fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/0.0.64.tar.gz";
      sha256 = "0mply6n3gym8r7z4l7512phw667qzwfqkfl1zmiyrdjpdg7xqn1d";
    })
    { pkgs = import nixpkgsSrc { inherit system; }; }

, iohkNix ? import
    (builtins.fetchTarball {
      url = "https://github.com/input-output-hk/iohk-nix/archive/d31417fe8c8fbfb697b3ad4c498e17eb046874b9.tar.gz";
      sha256 = "0w562wdmdhp83dw9rabiijj5hk1f4l8p8f3bwlr7virakgbg8lf8";
    })
    { }

  # NOTE: use the 'repo' branch of CHaP which contains the index
, CHaP ? (builtins.fetchTarball {
    url = "https://github.com/input-output-hk/cardano-haskell-packages/archive/ceaae5355c81453d7cb092acadec3441bf57ed11.tar.gz";
    sha256 = "0pwlfv2gx7h2z492bfhbr6pakidi5i8dzpc4094sb05i9rrgyq32";
  })

, nixpkgsSrc ? iohkNix.nixpkgs
}:
let
  pkgs = import nixpkgsSrc (haskellNix.nixpkgsArgs // {
    inherit system;
    overlays =
      # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
      haskellNix.overlays
        # needed for cardano-crypto-class which uses a patched libsodium
        ++ iohkNix.overlays.crypto;
  });

  hsPkgs = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "hydra";
      src = ./.;
    };
    projectFileName = "cabal.project";
    compiler-nix-name = compiler;

    inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };

    modules = [
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

  # Add cardano-node & cardano-cli for our shell environments.
  # This is stable as it doesn't mix dependencies with this code-base; the
  # fetched binaries are the "standard" builds that people test. This should be
  # fast as it mostly fetches Hydra (CI) caches without building much.
  cardano-node = import
    (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "1.35.4";
      sha256 = "1j01m2cp2vdcl26zx9xmipr551v3b2rz9kfn9ik8byfwj1z7652r";
    })
    { };
in
{
  inherit compiler pkgs hsPkgs cardano-node;
}
