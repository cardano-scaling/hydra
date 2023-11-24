{ pkgs ? import <nixpkgs> { }
, diagramFilterSource ? fetchTarball {
    url = "https://github.com/pandoc-ext/diagram/archive/v1.0.0.tar.gz";
    sha256 = "04d7krqn2wsa6ngblnjrc8j0b3sbx60q1kn4k17kcjzh5xmwkma3";
  }
}:
let
  buildScript = ./build.sh;
  fonts = pkgs.makeFontsConf {
    fontDirectories = [
      pkgs.dejavu_fonts
      pkgs.liberation_ttf
      pkgs.open-sans
      pkgs.roboto
    ];
  };
in
pkgs.stdenv.mkDerivation rec {
  name = "hydra-spec";
  src = ./.;
  buildInputs = with pkgs; [
    pandoc
    haskellPackages.pandoc-crossref
    # TODO: this is big, find a smaller working distribution
    texlive.combined.scheme-full
    nodePackages.mermaid-cli # mmdc
    librsvg # rsvg-convert
    inkscape
  ];
  phases = [ "unpackPhase" "buildPhase" ];
  buildPhase = ''
    export FONTCONFIG_FILE=${fonts}
    ln -s ${diagramFilterSource} diagram
    ${buildScript} $out
  '';
}
