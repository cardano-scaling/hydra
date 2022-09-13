# Hydra release jobset

{ ... }:
let
  project = import ./default.nix { };
in
{
  # Build shell derivation to cache it
  shell = import ./shell.nix { };

  # Build executables only (for now)
  hydra-node = project.hsPkgs.hydra-node.components.exes.hydra-node;
  hydra-tui = project.hsPkgs.hydra-tui.components.exes.hydra-tui;
  hydraw = project.hsPkgs.hydraw.components.exes;
}
