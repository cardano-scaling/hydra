# Hydra release jobset

{
  # this is passed in by hydra to provide us with the revision
  hydra-poc ? { outPath = ./.; rev = "abcdef"; }
, ...
}:
let
  project = import ./default.nix { };
in
rec {
  # Build shell derivation to cache it
  shell = import ./shell.nix { };

  # Build executables only (for now)
  hydra-node = project.hsPkgs.hydra-node.components.exes.hydra-node;
  hydra-tui = project.hsPkgs.hydra-tui.components.exes.hydra-tui;
  hydraw = project.hsPkgs.hydraw.components.exes.hydraw;

  # defines required jobs so that Hydra-the-CI correctly notifies
  # GH when the jobs are built
  # This is to prevent spurious GH Actions failures caused by Hydra
  required = project.pkgs.releaseTools.aggregate {
    name = "github-required";
    meta.description = "All jobs required to pass CI";
    constituents = [
      hydra-node
      hydra-tui
      hydraw
      shell
      # Added to be sure that hydra notify even if there is no change in above jobs:
      (project.pkgs.writeText "revision.txt" hydra-poc.rev)
    ];
  };


}
