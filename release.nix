# Hydra release jobset

{ pkgs ? import <nixpkgs> { } }:
{
  # Keep this until we have a proper job
  hello = pkgs.hello;

  # TODO(SN): Build shell derivation to cache it, but mkShell can't be built?
  shell = ./shell.nix { };
}
