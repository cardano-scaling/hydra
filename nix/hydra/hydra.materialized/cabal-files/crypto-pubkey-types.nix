{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.6";
      identifier = { name = "crypto-pubkey-types"; version = "0.4.3"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Vincent Hanquez <vincent@snarc.org>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "http://github.com/vincenthz/hs-crypto-pubkey-types";
      url = "";
      synopsis = "Generic cryptography Public keys algorithm types";
      description = "Generic cryptography public keys algorithm types";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
          (hsPkgs."asn1-encoding" or (errorHandler.buildDepError "asn1-encoding"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/crypto-pubkey-types-0.4.3.tar.gz";
      sha256 = "7ed9f52281ec4e34021a91818fe45288e33d65bff937f60334a3f45be5a71c60";
      });
    }) // {
    package-description-override = "Name:                crypto-pubkey-types\nVersion:             0.4.3\nDescription:         Generic cryptography public keys algorithm types\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           Vincent Hanquez <vincent@snarc.org>\nAuthor:              Vincent Hanquez <vincent@snarc.org>\nMaintainer:          Vincent Hanquez <vincent@snarc.org>\nSynopsis:            Generic cryptography Public keys algorithm types\nCategory:            Cryptography\nBuild-Type:          Simple\nHomepage:            http://github.com/vincenthz/hs-crypto-pubkey-types\nCabal-Version:       >=1.6\n\nLibrary\n  Exposed-modules:   Crypto.Types.PubKey.RSA\n                     Crypto.Types.PubKey.DSA\n                     Crypto.Types.PubKey.DH\n                     Crypto.Types.PubKey.ECC\n                     Crypto.Types.PubKey.ECDSA\n  Build-depends:     base >= 4 && < 5\n                   , asn1-types >= 0.1 && < 0.4\n                   , asn1-encoding\n\nsource-repository head\n  type:     git\n  location: git://github.com/vincenthz/hs-crypto-pubkey-types\n";
    }