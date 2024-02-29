{ buildYarnPackage }:

(buildYarnPackage rec {
  src = ./.;
  yarnBuildMore = "yarn build";
}).overrideAttrs (oldAttrs: {
  installPhase = ''
    mkdir -p $out
    cp -r out/* $out
  '';
})
