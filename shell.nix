# A shell setup providing build tools and utilities for a development
# environment. The main shell environment is based on haskell.nix and uses the
# same nixpkgs as the default nix builds (see default.nix).

{
  # Used in CI to have a smaller closure
  withoutDevTools ? false
}:
let
  project = import ./default.nix { };

  inherit (project) pkgs hsPkgs compiler;

  # Add cardano-node & cardano-cli for our shell environment.
  # This is stable as it doesn't mix dependencies with this code-base; the
  # fetched binaries are the "standard" builds that people test. This should be
  # fast as it mostly fetches Hydra (CI) caches without building much.
  cardano-node = import
    (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "1.35.0";
      sha256 = "06arx9hv7dn3qxfy83f0b6018rxbsvh841nvfyg5w6qclm1hddj7";
    })
    { };

  libs = [
    pkgs.glibcLocales
    pkgs.libsodium-vrf # from iohk-nix overlay
    pkgs.lzma
    pkgs.secp256k1
    pkgs.zlib
  ]
  ++
  pkgs.lib.optionals (pkgs.stdenv.isLinux) [ pkgs.systemd ];

  buildInputs = [
    pkgs.git
    pkgs.pkgconfig
    pkgs.haskellPackages.hspec-discover
    pkgs.haskellPackages.cabal-plan
    # For validating JSON instances against a pre-defined schema
    pkgs.python3Packages.jsonschema
    # For plotting results of hydra-cluster benchmarks
    pkgs.gnuplot
    # For integration tests
    cardano-node.cardano-node
  ];

  devInputs = if withoutDevTools then [ ] else [
    # The interactive Glasgow Haskell Compiler as a Daemon
    pkgs.haskellPackages.ghcid
    # Generate a graph of the module dependencies in the "dot" format
    pkgs.haskellPackages.graphmod
    # Automagically format .cabal files
    pkgs.haskellPackages.cabal-fmt
    # Handy to interact with the hydra-node via websockets
    pkgs.ws
    # Like 'jq' to manipulate JSON, but work for YAML
    pkgs.yq
    # For docs/ (i.e. Docusaurus, Node.js & React)
    pkgs.yarn
    # To interact with cardano-node and testing out things
    cardano-node.cardano-cli
  ];

  # Haskell.nix managed tools (via hackage)
  buildTools = {
    cabal = "3.4.0.0";
  };

  devTools = if withoutDevTools then { } else {
    fourmolu = "0.4.0.0"; # 0.5.0.0 requires Cabal 3.6
    haskell-language-server = "latest";
  };

  haskellNixShell = project.hsPkgs.shellFor {
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
      plutus-cbor
      plutus-merkle-tree
    ];

    tools = buildTools // devTools;

    buildInputs = libs ++ buildInputs ++ devInputs;

    withHoogle = !withoutDevTools;

    # Always create missing golden files
    CREATE_MISSING_GOLDEN = 1;

    # Force a UTF-8 locale because many Haskell programs and tests
    # assume this.
    LANG = "en_US.UTF-8";

    GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  };

  # A "cabal-only" shell which does not use haskell.nix
  cabalShell = pkgs.mkShell {
    name = "hydra-node-cabal-shell";

    buildInputs = libs ++ [
      pkgs.haskell.compiler.${compiler}
      pkgs.cabal-install
      pkgs.pkgconfig
    ] ++ buildInputs ++ devInputs;

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

  # A shell which does provide hydra-node and hydra-cluster executables.
  exeShell = pkgs.mkShell {
    name = "hydra-node-exe-shell";

    buildInputs = [
      cardano-node.cardano-node
      cardano-node.cardano-cli
      hsPkgs.hydra-node.components.exes.hydra-node
      hsPkgs.hydra-cluster.components.exes.hydra-cluster
    ];
  };
in
haskellNixShell // {
  cabalOnly = cabalShell;
  exes = exeShell;
}
