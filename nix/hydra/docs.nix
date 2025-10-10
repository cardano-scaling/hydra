{ self, inputs, ... }: {
  perSystem = { self', pkgs, lib, system, ... }: {
    packages = rec {
      docs =
        let
          src = pkgs.stdenv.mkDerivation {
            name = "hydra-docs-source";
            dontBuild = true;
            dontUnpack = true;
            installPhase = ''
              mkdir -p $out
              mkdir -p $out/static
              cp -r ${lib.cleanSource "${self}/docs"}/* $out/
              cp ${inputs.hydra-spec.packages.${system}.default}/hydra-spec.pdf $out/static
              cp -r ${self'.packages.haddocks} $out/static/haddocks
            '';
          };

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
        pkgs.buildYarnPackage rec {
          inherit src;
          yarnBuildMore = "yarn build";
          nativeBuildInputs = [ gitWrapper ];
        };

      docs-unstable = docs.overrideAttrs {
        configurePhase = ''
          sed -i '/^const BASE_URL/s|head-protocol|head-protocol/unstable|' docusaurus.config.js
        '';
      };
    };
  };
}
