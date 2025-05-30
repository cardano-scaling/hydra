{ self, ... }: {
  perSystem = { pkgs, ... }: {
    packages.tx-cost-diff =
      pkgs.writers.writeHaskellBin
        "tx-cost-diff"
        {
          libraries =
            with pkgs.haskellPackages;
            [ aeson text bytestring lens lens-aeson shh ];
        } ''${builtins.readFile "${self}/scripts/tx-cost-diff.hs"}'';
  };
}
