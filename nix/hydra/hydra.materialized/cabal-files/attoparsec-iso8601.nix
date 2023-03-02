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
    flags = { developer = false; fast = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "attoparsec-iso8601"; version = "1.0.2.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2011-2016 Bryan O'Sullivan\n(c) 2011 MailRank, Inc.";
      maintainer = "Adam Bergmark <adam@bergmark.nl>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/haskell/aeson";
      url = "";
      synopsis = "Parsing of ISO 8601 dates, originally from aeson.";
      description = "Parsing of ISO 8601 dates, originally from aeson.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
          (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/attoparsec-iso8601-1.0.2.0.tar.gz";
      sha256 = "02952d77c78e95710eea855f4e86ca048ab9fda83c6c08dd9215f21a40604f98";
      });
    }) // {
    package-description-override = "name:            attoparsec-iso8601\nversion:         1.0.2.0\nx-revision: 3\nsynopsis:        Parsing of ISO 8601 dates, originally from aeson.\ndescription:     Parsing of ISO 8601 dates, originally from aeson.\nlicense:         BSD3\nlicense-file:    LICENSE\ncategory:        Parsing\ncopyright:       (c) 2011-2016 Bryan O'Sullivan\n                 (c) 2011 MailRank, Inc.\nauthor:          Bryan O'Sullivan <bos@serpentine.com>\nmaintainer:      Adam Bergmark <adam@bergmark.nl>\nstability:       experimental\ncabal-version:   >=1.10\nhomepage:        https://github.com/haskell/aeson\nbug-reports:     https://github.com/haskell/aeson/issues\nbuild-type:      Simple\ntested-with:     GHC == 7.8.4, GHC == 7.10.3, GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5, GHC == 8.8.3, GHC == 8.10.1\nextra-source-files:\n  README.md\n\nflag developer\n  description: operate in developer mode\n  default: False\n  manual: True\n\nflag fast\n  description: compile without optimizations\n  default: False\n  manual: True\n\nlibrary\n  hs-source-dirs:      .\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n  exposed-modules:\n    Data.Attoparsec.Time.Internal\n    Data.Attoparsec.Time\n  build-depends:\n    attoparsec >= 0.13.1.0 && < 0.15,\n    base >= 4.7 && < 5,\n    base-compat-batteries >= 0.10.0 && < 0.13,\n    time-compat >= 1.9.4 && < 1.10,\n    text >= 1.1.1.0 && < 1.3.0.0,\n    time >= 1.1.1.4 && < 1.13\n\n  if flag(fast)\n    ghc-options: -O0\n  else\n    ghc-options: -O2\n\n  if flag(developer)\n    ghc-options: -Werror\n    ghc-prof-options: -auto-all\n\nsource-repository head\n  type:     git\n  location: git://github.com/haskell/aeson.git\n  subdir:   attoparsec-iso8601\n";
    }