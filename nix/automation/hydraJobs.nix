{
  cell,
  inputs,
}:

import "${inputs.self}/release.nix" {
  hydra = inputs.self;
  inherit (inputs.nixpkgs) system;
}
