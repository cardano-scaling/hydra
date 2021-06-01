# Hydra release jobset

{ ... }:
let
  sources = import ./nix/sources.nix { };
  iohkNix = import sources.iohk-nix { };
  nixpkgs = iohkNix.nixpkgs;
  pkgs = import nixpkgs { };
in
{
  # Keep this until we have a proper job
  hello = pkgs.hello;

  # TODO(SN): Build shell derivation to cache it, but mkShell can't be built?
  # shell = ./shell.nix { };
}
