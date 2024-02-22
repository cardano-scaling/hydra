# A shell setup providing build tools and utilities for a development
# environment. The main shell environment is based on haskell.nix and uses the
# same nixpkgs as the default nix builds (see default.nix).

{ hsPkgs
, inputs
, tools
, system
, pkgs
, compiler
}:
let

  buildInputs = pkgs.lib.mapAttrsToList (_: v: v) tools;

  libs = [
    pkgs.glibcLocales
    pkgs.libsodium-vrf # from iohk-nix overlay
    pkgs.lzma
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
      hsPkgs.compiler.${compiler}
      tools.cabal-install
      tools.pkg-config
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
      hsPkgs.hydra-node.components.exes.hydra-node
      hsPkgs.hydra-cluster.components.exes.hydra-cluster
      inputs.cardano-node.packages.${system}.cardano-node
      inputs.cardano-node.packages.${system}.cardano-cli
      inputs.mithril.packages.${system}.mithril-client-cli
    ];
  };

  # A shell setup providing build tools and utilities for the demo
  demoShell = pkgs.mkShell {
    name = "hydra-demo-shell";
    buildInputs = [
      hsPkgs.hydra-node.components.exes.hydra-node
      hsPkgs.hydra-tui.components.exes.hydra-tui
      run-tmux
      inputs.cardano-node.packages.${system}.cardano-node
      inputs.cardano-node.packages.${system}.cardano-cli
    ];
  };

  fmtShell = pkgs.mkShell {
    name = "hydra-format-shell";
    buildInputs = [
      tools.treefmt
      tools.fourmolu
      tools.cabal-fmt
      tools.nixpkgs-fmt
      tools.hlint
      tools.apply-refact
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
  fmt = fmtShell;
}
