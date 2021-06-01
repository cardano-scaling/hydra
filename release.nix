# Hydra release jobset

{ ... }:
{
  # Build shell derivation to cache it
  shell = import ./shell.nix { };
}
