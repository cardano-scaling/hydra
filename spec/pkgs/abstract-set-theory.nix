{ mkDerivation, fetchFromGitHub, standard-library, standard-library-classes, standard-library-meta }:

mkDerivation rec {
  version = "0.1.0";
  pname = "abstract-set-theory";

  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "agda-sets";
    rev = "751b3ee39122fe33022af41e6c94dc820afde19a";
    sha256 = "sha256-0vjmNN8wuZXT4NM3aEv4z1Y+/6LpNfb7vzeajwZ3eFY=";
  };

  buildInputs = [ standard-library standard-library-classes standard-library-meta ];

  everythingFile = "src/abstract-set-theory.agda";

  libraryFile = "abstract-set-theory.agda-lib";

  meta = { };
}

