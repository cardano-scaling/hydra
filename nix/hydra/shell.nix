# A shell setup providing build tools and utilities for a development
# environment. The main shell environment is based on haskell.nix and uses the
# same nixpkgs as the default nix builds (see default.nix).

{ hsPkgs
, inputs
, system
, pkgs
, ghc
, aiken
, hydraPackages
}:
let

  buildInputs = [
    # For running automatic refactoring with hlint
    pkgs.apply-refact
    pkgs.cabal-fmt
    pkgs.cabal-install
    # Handy tool to debug the cabal build plan
    pkgs.cabal-plan
    # To compile hydra scripts
    aiken.packages.${system}.aiken
    # To interact with cardano-node and integration tests
    pkgs.cardano-cli
    # For integration tests
    pkgs.cardano-node
    # For validating JSON instances against a pre-defined schema
    pkgs.check-jsonschema
    pkgs.fourmolu
    pkgs.git
    # For plotting results of hydra-cluster benchmarks
    pkgs.gnuplot
    pkgs.haskell-language-server
    pkgs.hlint
    pkgs.haskellPackages.hspec-discover
    # The interactive Glasgow Haskell Compiler as a Daemon
    pkgs.haskellPackages.ghcid
    # Generate a graph of the module dependencies in the "dot" format
    pkgs.haskellPackages.graphmod
    # To interact with mithril and integration tests
    pkgs.jq
    pkgs.mithril-client-cli
    pkgs.mithril-client-cli-unstable
    pkgs.nixpkgs-fmt
    pkgs.nodejs
    pkgs.pkg-config
    # For generating plantuml drawings
    pkgs.plantuml
    pkgs.treefmt
    # Handy to interact with the hydra-node via websockets
    pkgs.websocat
    pkgs.weeder
    pkgs.yarn
    pkgs.yq
  ];

  libs = [
    pkgs.glibcLocales
    pkgs.libsodium-vrf # from iohk-nix overlay
    pkgs.xz
    pkgs.secp256k1
    pkgs.zlib
  ]
  ++
  pkgs.lib.optionals (pkgs.stdenv.isLinux) [ pkgs.systemd ];

  haskellNixShell = hsPkgs.shellFor {
    buildInputs = libs ++ buildInputs;

    withHoogle = true;

    # Always create missing golden files
    CREATE_MISSING_GOLDEN = 1;

    # Force a UTF-8 locale because many Haskell programs and tests
    # assume this.
    LANG = "en_US.UTF-8";

    GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";

    shellHook = ''
      if ! which cardano-node > /dev/null 2>&1; then
        echo "WARNING: 'cardano-node' not found"
      fi
      if ! which cardano-cli > /dev/null 2>&1; then
        echo "WARNING: 'cardano-cli' not found"
      fi
    '';
  };

  # A "cabal-only" shell which does not use haskell.nix
  cabalShell = pkgs.mkShell {
    name = "hydra-node-cabal-shell";

    buildInputs = libs ++ [
      ghc
      pkgs.cabal-install
      pkgs.pkg-config
    ] ++ buildInputs;

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

  # A shell which does provide hydra-node and hydra-cluster executables along
  # with cardano-node, cardano-cli and mithril-client.
  exeShell = pkgs.mkShell {
    name = "hydra-node-exe-shell";

    buildInputs = [
      hydraPackages.hydra-node
      hydraPackages.hydra-cluster
      inputs.cardano-node.packages.${system}.cardano-node
      inputs.cardano-node.packages.${system}.cardano-cli
      pkgs.mithril-client-cli
      pkgs.mithril-client-cli-unstable
    ];
  };

  # A shell setup providing build tools and utilities for the demo
  demoShell = pkgs.mkShell {
    name = "hydra-demo-shell";
    buildInputs = [
      hydraPackages.hydra-node
      hydraPackages.hydra-tui
      run-tmux
      inputs.cardano-node.packages.${system}.cardano-node
      inputs.cardano-node.packages.${system}.cardano-cli
    ];
  };

  # Shell for CI activities
  ciShell = pkgs.mkShell {
    name = "hydra-ci-shell";
    buildInputs = [
      # For building docs
      pkgs.plantuml
      # Note: jq 1.6 has a bug that means it fails to read large integers
      # correctly, so we require 1.7+ at least.
      pkgs.jq
      pkgs.weeder
    ];
  };

  # Shell for computing tx-cost-differences
  costDifferencesShell = pkgs.mkShell {
    name = "tx-cost-differences-shell";
    buildInputs = [
      pkgs.pandoc
      (pkgs.python3.withPackages (ps: with ps; [ pandas html5lib beautifulsoup4 tabulate ]))
    ];
  };

  # If you want to modify `Python` code add `libtmux` and pyyaml to the
  # `buildInputs` then enter it and then run `Python` module directly so you
  # have fast devel cycle.
  run-tmux = pkgs.writers.writePython3Bin
    "run-tmux"
    { libraries = with pkgs.python3Packages; [ libtmux pyyaml ]; }
    # TODO: should use project relative path (via flake inputs.self?)
    (builtins.readFile ./../../demo/run-tmux.py);
in
{
  default = haskellNixShell;
  cabalOnly = cabalShell;
  exes = exeShell;
  demo = demoShell;
  ci = ciShell;
  costDifferences = costDifferencesShell;
}
