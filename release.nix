# Hydra release jobset

{ ... }:
let
  iohkNix = import
    (builtins.fetchTarball {
      url = "https://github.com/input-output-hk/iohk-nix/archive/8b1d65ba294708b12d7b15103ac35431d9b60819.tar.gz";
      sha256 = "1z23lw28s3wa5bf5yr89i61m413ad299lyhv02i9r36p28wjl94g";
    })
    { };
  pkgs = import iohkNix.nixpkgs { };
in
{
  # Keep this until we have a proper job
  hello = pkgs.hello;

  # TODO(SN): Build shell derivation to cache it, but mkShell can't be built?
  # shell = ./shell.nix { };
}
