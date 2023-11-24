{ pkgs ? import <nixpkgs> { }
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
  ];
  phases = [ "unpackPhase" "buildPhase" ];
  buildPhase = ''
    export FONTCONFIG_FILE=${fonts}
    ${buildScript} $out
  '';
}
