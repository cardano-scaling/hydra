{ self, ... }: {
  perSystem = { pkgs, ... }:
    {
      # Compares `tx-cost --json` output of this tree against another revision.
      # Both sides are structured, so no Markdown/HTML round-trip (and hence no
      # pandoc or pandas) is involved.
      packages.tx-cost-diff =
        pkgs.writers.writeHaskellBin
          "tx-cost-diff"
          {
            libraries =
              with pkgs.haskellPackages;
              [ aeson bytestring containers typed-process ];
          } ''${builtins.readFile "${self}/scripts/tx-cost-diff.hs"}'';
    };
}
