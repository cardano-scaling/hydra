{ inputs ? (builtins.getFlake "git+file://${toString ../..}").inputs
, system ? builtins.currentSystem
}:
let
  pkgs = import inputs.nixpkgs { inherit system; };
  accumulatorVersion = builtins.head (builtins.match
    ".*haskell-accumulator[[:space:]]+==([0-9.]+).*"
    (builtins.readFile ../../hydra-tx/hydra-tx.cabal));
  haskell = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9103.override {
    overrides = final: previous: {
      # Run singleton splices sequentially with the JavaScript compiler.
      singletons-base = pkgs.haskell.lib.overrideCabal previous.singletons-base {
        enableParallelBuilding = false;
      };
      crem = pkgs.haskell.lib.overrideCabal
        (pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck
          (final.callCabal2nix "crem" inputs.crem { })))
        {
          configureFlags = [ "lib:crem" ];
          isExecutable = false;
          enableParallelBuilding = false;
        };
    };
  };
  source = pkgs.lib.fileset.toSource {
    root = ../..;
    fileset = pkgs.lib.fileset.unions [
      ./head-playground-runtime.cabal
      ./src
      ./portable
      ../../hydra-node/src
      ../../hydra-tx/src
      ../../hydra-prelude/src
      ../../hydra-plutus/src
    ];
  };
in
pkgs.haskell.lib.overrideCabal
  (pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck
    (haskell.callCabal2nixWithOptions "head-playground-runtime" source
    {
      extraCabal2nixOptions = "--subpath docs/runtime";
      srcModifier = src: src;
    }
    { })))
{
  enableParallelBuilding = false;
  preConfigure = ''
    mkdir -p upstream
    tar -xzf ${inputs.CHaP}/package/haskell-accumulator-${accumulatorVersion}.tar.gz -C upstream --strip-components=1
  '';
}
