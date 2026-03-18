{ self, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.tx-cost-diff = pkgs.writers.writeHaskellBin "tx-cost-diff" {
        libraries = with pkgs.haskellPackages; [
          pandoc
          pandoc-types
          shh
          text
          bytestring
        ];
      } "${builtins.readFile "${self}/scripts/tx-cost-diff.hs"}";
    };
}
