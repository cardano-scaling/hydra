{ self, ... }: {
  perSystem = { self', pkgs, lib, ... }: {
    packages = rec {
      docs =
        let
          gitWrapper = pkgs.writeShellScriptBin "git" ''
            if [ "$1" = "--no-pager" ] && [ "$2" = "log" ] && [ "$3" = "-1" ] && [ "$4" = "--pretty=format:'%aI'" ]; then
              date --date="@${builtins.toString self.sourceInfo.lastModified}" +%DT%T
            elif [ "$1" = "--no-pager" ] && [ "$2" = "log" ] && [ "$3" = "-1" ] && [ "$4" = "--pretty=format:'%H'" ]; then
              echo "${if (self ? rev) then self.rev else self.sourceInfo.dirtyRev}"
            else
              echo "Pure Git Command Not Implemented: $@"
            fi
          '';
        in
        pkgs.buildYarnPackage {
          src = lib.cleanSource "${self}/docs";
          yarnBuildMore = "set -a; source yarn.env; set +a; yarn build";
          # XXX: Note that in principle we are missing `plantuml` binary here;
          # the yarn build tries to run it, but it doesn't have any impact
          # because we actually expect people to run this manually outside of
          # Nix.
          #
          # In theory one could use the `plantuml-c4` binary from nixpkgs,
          # and; but that seems to require additional changes to the actualy
          # architecture diagram file itself (architecture-c4.puml).
          nativeBuildInputs = [ gitWrapper ];
          # Inject the spec PDF and haddocks before yarn build runs.
          # These cannot be part of `src` directly because they are derivation
          # outputs, but they must be present for the docusaurus build.
          preBuild = ''
            mkdir -p static
            cp ${self'.packages.spec}/hydra-spec.pdf static/
            cp -rL ${self'.packages.haddocks} static/haddocks
            chmod -R u+w static/haddocks

            # Docs reference haddock pages as `pathname:///haddocks/...`. Those render
            # root-relative, so the markdown link checker skips them and a stale path
            # can rot unnoticed. Check each target against the tree just copied.
            missing=""
            for p in $(grep -rhoE 'pathname:///haddocks/[A-Za-z0-9._/-]+\.html' \
                         --include='*.md' --include='*.mdx' . \
                       | sed 's|pathname:///haddocks/||' | sort -u); do
              if [ ! -f "static/haddocks/$p" ]; then missing="$missing $p"; fi
            done
            if [ -n "$missing" ]; then
              echo "ERROR: docs link to haddock pages that do not exist:"
              for p in $missing; do echo "  $p"; done
              exit 1
            fi

            # Generate the transaction-cost benchmark page fresh from the current
            # code so it renders as a normal docusaurus page (current theme, no
            # staleness). Its on-chain costs are deterministic, so a fixed seed
            # keeps it reproducible and nix-cached. The output is git-ignored and
            # never committed.
            ${self'.packages.tx-cost}/bin/tx-cost --seed 42 --output-directory benchmarks
          '';
          # yarn pack (used by buildYarnPackage's installPhase) excludes the
          # build/ directory because it is listed in .gitignore. Copy it
          # explicitly so the workflow can find result/build/*.
          postInstall = ''
            cp -r build "$out/build"
          '';
        };

      docs-unstable = docs.overrideAttrs {
        configurePhase = ''
          sed -i '/^const BASE_URL/s|head-protocol|head-protocol/unstable|' docusaurus.config.js
        '';
      };
    };
  };
}
