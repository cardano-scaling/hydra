# Hydra release jobset

{ ... }:
let
  hsPkgs = import ./default.nix { };
in
{
  # Build shell derivation to cache it
  shell = import ./shell.nix { };

  # Build executables only (for now)
  hydra-node = hsPkgs.hydra-node.components.exes.hydra-node;
  hydra-tui = hsPkgs.hydra-tui.components.exes.hydra-tui;
}
