{ self, inputs, ... }: {
  perSystem = { self', pkgs, lib, ... }: {
    checks.head-playground = pkgs.runCommand "head-playground-check"
      {
        nativeBuildInputs = [ pkgs.nodejs ];
        HYDRA_PLAYGROUND_BUNDLE = "${self'.packages.head-playground-runtime}/bin/head-playground-runtime.jsexe/all.js";
      } ''
      node --test ${self}/docs/scripts/head-playground.test.mjs
      touch "$out"
    '';

    checks.head-logic-diagrams = pkgs.runCommand "head-logic-diagrams-check"
      {
        nativeBuildInputs = [ pkgs.diffutils ];
      } ''
      ${self'.packages.head-logic-diagrams}/bin/head-logic-diagrams generated.md
      diff -u ${self}/docs/docs/dev/architecture/head-logic-machines.md generated.md || {
        echo "HeadLogic diagrams are out of date."
        echo "From docs/, run 'yarn head-logic-diagrams' and commit the updated page."
        exit 1
      }
      touch "$out"
    '';

    packages = rec {
      head-playground-runtime = import ../../docs/runtime {
        inherit inputs;
        inherit (pkgs.stdenv.hostPlatform) system;
      };

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
          # yarn also generates the Crem page from the compiled topology types.
          HYDRA_HEAD_LOGIC_DIAGRAMS = "${self'.packages.head-logic-diagrams}/bin/head-logic-diagrams";
          HYDRA_SOURCE_REVISION = if (self ? rev) then self.rev else lib.removeSuffix "-dirty" self.sourceInfo.dirtyRev;
          nativeBuildInputs = [ gitWrapper ];
          # Inject the spec PDF and haddocks before yarn build runs.
          # These cannot be part of `src` directly because they are derivation
          # outputs, but they must be present for the docusaurus build.
          preBuild = ''
            mkdir -p static
            mkdir -p static/head-playground-runtime/compiled
            cp ${head-playground-runtime}/bin/head-playground-runtime.jsexe/all.js static/head-playground-runtime/compiled/
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
