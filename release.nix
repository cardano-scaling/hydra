# Hydra release jobset

{ pkgs ? import
    (builtins.fetchTarball
      "https://github.com/nixos/nixpkgs/archive/1232484e2bbd1c84f968010ea5043a73111f7549.tar.gz")
    { }
}:
{
  # Keep this until we have a proper job
  hello = pkgs.hello;

  # TODO(SN): Build shell derivation to cache it, but mkShell can't be built?
  # shell = ./shell.nix { };
}
