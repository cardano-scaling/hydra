# A shell setup providing build tools and utilities for a development
# environment. The main shell environment is based on haskell.nix and uses the
# same nixpkgs as the default nix builds (see default.nix).

{
  # Used in CI to have a smaller closure
  withoutDevTools ? false
, hydraProject
, cardano-node
, system ? builtins.currentSystem
}:
let
  inherit (hydraProject) compiler pkgs hsPkgs;

  cardano-node-pkgs = cardano-node.packages.${system};

  cabal = pkgs.haskell-nix.cabal-install.${compiler};

  haskell-language-server = pkgs.haskell-nix.tool compiler "haskell-language-server" rec {
    src = pkgs.haskell-nix.sources."hls-1.10";
    cabalProject = builtins.readFile (src + "/cabal.project");
    sha256map."https://github.com/pepeiborra/ekg-json"."7a0af7a8fd38045fd15fb13445bdcc7085325460" = "sha256-fVwKxGgM0S4Kv/4egVAAiAjV7QB5PBqMVMCfsv7otIQ=";
  };

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
    # Build essentials
    pkgs.git
    pkgs.pkgconfig
    cabal
    pkgs.haskellPackages.hspec-discover
    pkgs.haskellPackages.cabal-plan
    # For validating JSON instances against a pre-defined schema
    pkgs.python3Packages.jsonschema
    # For plotting results of hydra-cluster benchmarks
    pkgs.gnuplot
    # For integration tests
    cardano-node-pkgs.cardano-node
  ];

  devInputs = if withoutDevTools then [ ] else [
    # Automagically format .hs and .cabal files
    pkgs.haskellPackages.fourmolu
    pkgs.haskellPackages.cabal-fmt
    # Essenetial for a good IDE
    haskell-language-server
    # The interactive Glasgow Haskell Compiler as a Daemon
    pkgs.haskellPackages.ghcid
    # Generate a graph of the module dependencies in the "dot" format
    pkgs.haskellPackages.graphmod
    # Handy to interact with the hydra-node via websockets
    pkgs.websocat
    # Like 'jq' to manipulate JSON, but work for YAML
    pkgs.yq
    # For docs/ (i.e. Docusaurus, Node.js & React)
    pkgs.yarn
    pkgs.nodejs
    # To interact with cardano-node and testing out things
    cardano-node-pkgs.cardano-cli
  ];

  haskellNixShell = hsPkgs.shellFor {
    # NOTE: Explicit list of local packages as hoogle would not work otherwise.
    # Make sure these are consistent with the packages in cabal.project.
    packages = ps: with ps; [
      hydra-prelude
      hydra-cardano-api
      hydra-test-utils
      plutus-cbor
      plutus-merkle-tree
      hydra-plutus
      # hydra-node
      # hydra-cluster
      # hydra-tui
      # hydraw
    ];

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
      cardano-node-pkgs.cardano-node
      cardano-node-pkgs.cardano-cli
      hsPkgs.hydra-node.components.exes.hydra-node
      hsPkgs.hydra-cluster.components.exes.hydra-cluster
    ];
  };

  # A shell setup providing build tools and utilities for the demo
  demoShell = pkgs.mkShell {
    name = "hydra-demo-shell";
    buildInputs = [
      cardano-node-pkgs.cardano-node
      cardano-node-pkgs.cardano-cli
      hsPkgs.hydra-node.components.exes.hydra-node
      hsPkgs.hydra-tui.components.exes.hydra-tui
      run-tmux
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
}
