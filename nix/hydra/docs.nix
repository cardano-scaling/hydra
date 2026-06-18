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
