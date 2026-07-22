{ mkDerivation, fetchFromGitHub, abstract-set-theory, standard-library, standard-library-classes, standard-library-meta }:

mkDerivation rec {
  version = "0.2.0";
  pname = "formal-ledger";

  src = (fetchFromGitHub {
    owner = "IntersectMBO";
    repo = "formal-ledger-specifications";
    rev = "cef3505f275900c51737071b5671e5dad47dc576";
    sha256 = "sha256-KAyojB3uClt72fhOzw9iDSCGzlL8EUhEtK/Xizj5pyU=";
  }) + "/src";

  buildInputs = [ abstract-set-theory standard-library standard-library-classes standard-library-meta ];

  meta = { };
}

