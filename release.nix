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
  mock-chain = hsPkgs.hydra-node.components.exes.mock-chain;
  hydra-pab = hsPkgs.hydra-plutus.components.exes.hydra-pab;
}
