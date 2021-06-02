{ compiler ? "ghc8104"
, haskellNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/haskell.nix/archive/532e71470b41fc0fd0d3d858ea98d7f07f37d309.tar.gz")
    { }
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgsSrc nixpkgsArgs
}:
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "haskell-poc";
    src = ./.;
  };
  compiler-nix-name = compiler;
}
