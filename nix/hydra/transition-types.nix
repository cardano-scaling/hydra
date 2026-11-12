_: {
  perSystem = { pkgs, hsPkgs, ... }: {
    checks.head-logic-transition-types = pkgs.runCommand "head-logic-transition-types"
      { nativeBuildInputs = [ (hsPkgs.ghcWithPackages (hs: [ hs.hydra-node hs.crem ])) ]; }
      ''
        bash ${../../scripts/check-head-logic-types.sh} --ghc ${../../hydra-node/typecheck}
        touch "$out"
      '';
  };
}
