# Hydra release jobset

{
  # this is passed in by hydra to provide us with the revision
  hydra-poc ? { outPath = ./.; rev = "abcdef"; }
, ...
}:
let
  project = import ./default.nix { };
  nativePkgs = project.hsPkgs;
  # Allow reinstallation of terminfo as it's not installed with cross compilers.
  patchedForCrossProject = project.hsPkgs.appendModule
      ({ lib, ...}: { options.nonReinstallablePkgs = lib.mkOption { apply = lib.remove "terminfo"; }; });
  musl64Pkgs = patchedForCrossProject.projectCross.musl64.hsPkgs;
in
rec {
  # Build shell derivation to cache it
  shell = import ./shell.nix { };

  # Build some executables
  hydra-node = nativePkgs.hydra-node.components.exes.hydra-node;
  hydra-node-static = musl64Pkgs.hydra-node.components.exes.hydra-node;
  hydra-tui = nativePkgs.hydra-tui.components.exes.hydra-tui;
  hydra-tui-static = musl64Pkgs.hydra-tui.components.exes.hydra-tui;
  hydraw = nativePkgs.hydraw.components.exes.hydraw;

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
