let
  project = import ./default.nix { };
  inherit (project) pkgs hydra cardano-node;

  libs = [
    pkgs.libsodium-vrf
    pkgs.systemd
    pkgs.zlib
    pkgs.lzma
    pkgs.zeromq
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
    # Used in local-cluster
    cardano-node.cardano-node
    cardano-node.cardano-cli
    # For validating JSON instances against a pre-defined schema
    pkgs.python3Packages.jsonschema
    pkgs.yq
    # For plotting results of local-cluster benchmarks
    pkgs.gnuplot
  ];

  haskellNixShell = hydra.shellFor {
    # Haskell.nix managed tools (via hackage)
    tools = {
      cabal = "3.4.0.0";
      fourmolu = "latest";
      haskell-language-server = "latest";
    };

    nativeBuildInputs = tools;

    # Disable haddocks as it's currently failing for the 'plutus-ledger' package
    withHoogle = false;

    # Always create missing golden files
    CREATE_MISSING_GOLDEN = 1;
  };

  # A "cabal-only" shell which does not use haskell.nix
  cabalShell = pkgs.mkShell {
    name = "hydra-node-cabal-shell";

    buildInputs = libs;
    nativeBuildInputs = [
      (hydra.ghcWithPackages (ps :[]))
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
