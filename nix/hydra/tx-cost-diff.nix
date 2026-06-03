{ self, ... }: {
  perSystem = { pkgs, ... }:
    let
      tx-cost-diff =
        pkgs.writers.writeHaskellBin
          "tx-cost-diff"
          {
            libraries =
              with pkgs.haskellPackages;
              [ aeson text bytestring lens lens-aeson shh ];
          } ''${builtins.readFile "${self}/scripts/tx-cost-diff.hs"}'';

      pythonEnv = pkgs.python3.withPackages (ps: with ps; [
        pandas
        html5lib
        beautifulsoup4
        tabulate
      ]);
    in
    {
      packages.tx-cost-diff = pkgs.writeShellApplication {
        name = "tx-cost-diff";
        runtimeInputs = [ pkgs.pandoc pythonEnv ];
        text = ''exec ${tx-cost-diff}/bin/tx-cost-diff "$@"'';
      };
    };
}
