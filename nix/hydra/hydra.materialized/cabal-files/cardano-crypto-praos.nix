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
    flags = { development = false; external-libsodium-vrf = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cardano-crypto-praos"; version = "2.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Crypto primitives from libsodium";
      description = "VRF (and KES, tba) primitives from libsodium.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          ];
        pkgconfig = [
          (pkgconfPkgs."libsodium" or (errorHandler.pkgConfDepError "libsodium"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-crypto-praos-2.0.0.tar.gz";
      sha256 = "88afa32a0dc35402f545a89a163a7b46ec7c7c395575067bd2ea8307d0bf054b";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                cardano-crypto-praos\nversion:             2.0.0\nsynopsis:            Crypto primitives from libsodium\ndescription:         VRF (and KES, tba) primitives from libsodium.\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\nauthor:              IOHK\nmaintainer:          operations@iohk.io\ncopyright:           2019-2021 IOHK\ncategory:            Currency\nbuild-type:          Simple\nextra-source-files:  README.md\n\nextra-source-files:    cbits/crypto_vrf.h\n\n                       cbits/vrf03/crypto_vrf_ietfdraft03.h\n                       cbits/vrf03/vrf_ietfdraft03.h\n\n                       -- cbits/vrf10_batchcompat/crypto_vrf_ietfdraft10.h\n                       -- cbits/vrf10_batchcompat/vrf_ietfdraft10.h\n\n                       cbits/private/common.h\n                       cbits/private/quirks.h\n                       cbits/private/ed25519_ref10.h\n                       --cbits/private/hash_to_curve.h\n                       cbits/private/ed25519_ref10_fe_25_5.h\n                       cbits/private/ed25519_ref10_fe_51.h\n\n                       cbits/private/fe_25_5/constants.h\n                       cbits/private/fe_25_5/base.h\n                       cbits/private/fe_25_5/base2.h\n                       cbits/private/fe_25_5/fe.h\n                       cbits/private/fe_51/constants.h\n                       cbits/private/fe_51/base.h\n                       cbits/private/fe_51/base2.h\n                       cbits/private/fe_51/fe.h\n\nflag development\n    description: Disable `-Werror`\n    default: False\n    manual: True\n\nflag external-libsodium-vrf\n    description: Rely on a special libsodium fork containing the VRF code.\n                 Otherwise expect a normal unaltered system libsodium, and\n                 bundle the VRF code.\n    default: True\n    manual: True\n\ncommon base                         { build-depends: base                             >= 4.14       && < 4.15     }\n\ncommon project-config\n  default-language:     Haskell2010\n\n  ghc-options:          -Wall\n                        -Wcompat\n                        -Wincomplete-record-updates\n                        -Wincomplete-uni-patterns\n                        -Wpartial-fields\n                        -Wredundant-constraints\n                        -Wunused-packages\n\n  if (!flag(development))\n    ghc-options:        -Werror\n\nlibrary\n  import:               base, project-config\n  hs-source-dirs:       src\n  exposed-modules:      Cardano.Crypto.VRF.Praos\n                 -- Disabled until the full audit is complete:\n                 -- ,      Cardano.Crypto.VRF.PraosBatchCompat\n                 ,      Cardano.Crypto.RandomBytes\n\n  build-depends:        base\n                      , bytestring\n                      , cardano-binary\n                      , cardano-crypto-class\n                      , cardano-prelude\n                      , nothunks\n\n  pkgconfig-depends: libsodium\n\n  if !flag(external-libsodium-vrf)\n    c-sources:          cbits/crypto_vrf.c\n                        cbits/vrf03/convert.c\n                        cbits/vrf03/keypair.c\n                        cbits/vrf03/prove.c\n                        cbits/vrf03/verify.c\n                        cbits/vrf03/vrf.c\n\n                        -- cbits/vrf10_batchcompat/convert.c\n                        -- cbits/vrf10_batchcompat/verify.c\n                        -- cbits/vrf10_batchcompat/keypair.c\n                        -- cbits/vrf10_batchcompat/prove.c\n                        -- cbits/vrf10_batchcompat/vrf.c\n\n                        -- cbits/private/hash_to_curve.c\n                        cbits/private/ed25519_ref10.c\n";
    }