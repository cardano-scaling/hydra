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
      identifier = { name = "statistics-linreg"; version = "0.3"; };
      license = "MIT";
      copyright = "2010-2014 Alp Mestanogullari";
      maintainer = "Alp Mestanogullari <alpmestan@gmail.com>";
      author = "Alp Mestanogullari <alpmestan@gmail.com>, Uri Barenholz <uri.barenholz@weizmann.ac.il>";
      homepage = "http://github.com/alpmestan/statistics-linreg";
      url = "";
      synopsis = "Linear regression between two samples, based on the 'statistics' package.";
      description = "Provides functions to perform a linear regression between 2 samples, see the documentation of the linearRegression functions. This library is based on the 'statistics' package.\n\n* 0.3: you can now use all functions on any instance of the Vector class (not just unboxed vectors).\n\n* 0.2.4: added distribution estimations for standard regression parameters.\n\n* 0.2.3: added robust-fit support.\n\n* 0.2.2: added the Total-Least-Squares version and made some refactoring to eliminate code duplication\n\n* 0.2.1: added the r-squared version and improved the performances.\n\nCode sample:\n\n> import qualified Data.Vector.Unboxed as U\n>\n> test :: Int -> IO ()\n> test k = do\n>   let n = 10000000\n>   let a = k*n + 1\n>   let b = (k+1)*n\n>   let xs = U.fromList [a..b]\n>   let ys = U.map (\\x -> x*100 + 2000) xs\n>   -- thus 100 and 2000 are the alpha and beta we want\n>   putStrLn \"linearRegression:\"\n>   print $ linearRegression xs ys\n\nThe r-squared and Total-Least-Squares versions work the same way.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."statistics" or (errorHandler.buildDepError "statistics"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."MonadRandom" or (errorHandler.buildDepError "MonadRandom"))
          (hsPkgs."random-shuffle" or (errorHandler.buildDepError "random-shuffle"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/statistics-linreg-0.3.tar.gz";
      sha256 = "6273c2166b8072814ede516c7c9f1e13a158b5013938fdf83a3ea2355aee8909";
      });
    }) // {
    package-description-override = "Name:                statistics-linreg\nVersion:             0.3\nSynopsis:            Linear regression between two samples, based on the 'statistics' package.\nDescription:         Provides functions to perform a linear regression between 2 samples, see the documentation of the linearRegression functions. This library is based on the 'statistics' package.\n\t\t     .\n           * 0.3: you can now use all functions on any instance of the Vector class (not just unboxed vectors).\n         .\n\t\t       * 0.2.4: added distribution estimations for standard regression parameters.\n\t\t     .\n\t\t       * 0.2.3: added robust-fit support.\n\t\t     .\n\t\t       * 0.2.2: added the Total-Least-Squares version and made some refactoring to eliminate code duplication\n\t\t     .\n\t\t       * 0.2.1: added the r-squared version and improved the performances.\n                     .\n                     Code sample:\n                     .\n                     > import qualified Data.Vector.Unboxed as U\n                     > \n                     > test :: Int -> IO ()\n                     > test k = do\n                     >   let n = 10000000\n                     >   let a = k*n + 1\n                     >   let b = (k+1)*n\n                     >   let xs = U.fromList [a..b]\n                     >   let ys = U.map (\\x -> x*100 + 2000) xs \n                     >   -- thus 100 and 2000 are the alpha and beta we want\n                     >   putStrLn \"linearRegression:\"\n                     >   print $ linearRegression xs ys\n                     .\n                     The r-squared and Total-Least-Squares versions work the same way.\nHomepage:            http://github.com/alpmestan/statistics-linreg\nBug-reports:         https://github.com/alpmestan/statistics-linreg/issues\nLicense:             MIT\nLicense-file:        LICENSE\nAuthor:              Alp Mestanogullari <alpmestan@gmail.com>, Uri Barenholz <uri.barenholz@weizmann.ac.il>\nMaintainer:          Alp Mestanogullari <alpmestan@gmail.com>\nCopyright:           2010-2014 Alp Mestanogullari\nStability:           Experimental\nCategory:            Math, Statistics\nBuild-type:          Simple\nCabal-version:       >=1.6\n\nLibrary\n  Exposed-modules: Statistics.LinearRegression\n  Build-depends:    statistics >= 0.5, vector >= 0.5, base >= 4 && < 5,\n                    safe >= 0.3, random >= 1.0, MonadRandom >= 0.1,\n                    random-shuffle >= 0.0.4\n  Ghc-options: -funbox-strict-fields -O2\n  \nsource-repository head\n  type: git\n  location: http://github.com/alpmestan/statistics-linreg.git\n";
    }