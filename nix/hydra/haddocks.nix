_: {
  perSystem = { pkgs, hsPkgs, ... }: {
    packages.haddocks = pkgs.runCommand "hydra-haddocks"
      {
        paths = [
          hsPkgs.hydra-prelude.components.library.doc
          hsPkgs.hydra-cardano-api.components.library.doc
          hsPkgs.hydra-plutus.components.library.doc
          hsPkgs.hydra-node.components.library.doc
          hsPkgs.hydra-tx.components.library.doc
          hsPkgs.hydra-tx.components.tests.tests.doc
          hsPkgs.hydra-node.components.tests.tests.doc
          hsPkgs.hydra-cluster.components.library.doc
          hsPkgs.hydra-tui.components.library.doc
        ];
      }
      ''
        set -ex
        mkdir -p $out
        for p in $paths; do
          cd $p
          for html in $(find $haddockRoot -name html -type d); do
            package=$(basename $(dirname $html))
            mkdir -p $out/$package
            cp -a $html/* $out/$package/
          done
        done
      '';
  };
}
