# A shell setup providing build tools and utilities for the demo
let
  project = import ../default.nix { };

  inherit (project) compiler pkgs hsPkgs cardano-node;

  # If you want to modify `Python` code add `libtmux` and pyyaml to the
  # `buildInputs` then enter it and then run `Python` module directly so you
  # have fast devel cycle.
  run-tmux = pkgs.writers.writePython3Bin
    "run-tmux"
    { libraries = with pkgs.python3Packages; [libtmux pyyaml]; }
    (builtins.readFile ./run-tmux.py);
in
pkgs.mkShell {
  name = "hydra-demo-shell";
  buildInputs = [
    cardano-node.cardano-node
    cardano-node.cardano-cli
    hsPkgs.hydra-node.components.exes.hydra-node
    hsPkgs.hydra-tui.components.exes.hydra-tui
    run-tmux
  ];
}
