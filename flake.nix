{
  inputs = {
    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    aiken.url = "github:aiken-lang/aiken/v1.1.19";
    cardano-node.url = "github:intersectmbo/cardano-node/10.4.1";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    hydra-coding-standards.url = "github:cardano-scaling/hydra-coding-standards/0.6.5";
    hydra-spec.url = "github:cardano-scaling/hydra-formal-specification/7f78e005b95ff3b9c55995632ceb3d6ab96a305e";
    import-tree.url = "github:vic/import-tree";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    mithril.url = "github:input-output-hk/mithril/2524.0";
    nixpkgs.follows = "haskellNix/nixpkgs";
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./nix);

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cardano-scaling.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cardano-scaling.cachix.org-1:QNK4nFrowZ/aIJMCBsE35m+O70fV6eewsBNdQnCSMKA="
    ];
    allow-import-from-derivation = true;
  };
}
