# Overlay vendored from https://github.com/input-output-hk/haskell-nix-example/blob/2247d445b91b0cc21eda4359e060d76b3cc0ce41/flake.nix#L53C17-L53C17
{
  flake.overlays.static-libs =
    final: _prev: {
      static-libsodium-vrf = final.libsodium-vrf.overrideDerivation (old: {
        configureFlags = old.configureFlags ++ [ "--disable-shared" ];
      });
      static-secp256k1 = final.secp256k1.overrideDerivation (old: {
        configureFlags = old.configureFlags ++ [ "--enable-static" "--disable-shared" ];
      });
      static-gmp = (final.gmp.override { withStatic = true; }).overrideDerivation (old: {
        configureFlags = old.configureFlags ++ [ "--enable-static" "--disable-shared" ];
      });
      static-libblst = (final.libblst.override { enableShared = false; }).overrideDerivation (_old: {
        postFixup = "";
      });
      static-openssl = final.openssl.override { static = true; };
      static-zlib = final.zlib.override { shared = false; };
      static-pcre = final.pcre.override { shared = false; };
      # XXX: Not replicate the cmakeFlags but just drop the -DBUILD_SHARED_LIBS=ON from it
      static-snappy = final.snappy.overrideDerivation (_old: {
        cmakeFlags = [ "-DSNAPPY_BUILD_TESTS=OFF" "-DSNAPPY_BUILD_BENCHMARKS=OFF" ];
      });
    };
}
