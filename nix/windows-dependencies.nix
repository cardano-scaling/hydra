{ inputs, ... }: {
  flake.overlays.windows-dependencies = final: prev:
    prev.lib.optionalAttrs prev.stdenv.hostPlatform.isWindows {
      librust_accumulator = final.rustPlatform.buildRustPackage {
        pname = "rust-accumulator";
        version = "0.1.0";
        src = "${inputs.rust-accumulator}/rust_accumulator";
        cargoLock.lockFile = "${inputs.rust-accumulator}/rust_accumulator/Cargo.lock";
        cargoBuildFlags = [ "--lib" ];
        doCheck = false;
        postInstall = ''
          mkdir -p $out/lib/pkgconfig $out/include
          cp -r ${inputs.rust-accumulator}/include/. $out/include/
          cat > $out/lib/pkgconfig/librust_accumulator.pc <<EOF
          prefix=$out
          libdir=\''${prefix}/lib
          includedir=\''${prefix}/include

          Name: librust_accumulator
          Description: Rust accumulator library
          Version: 0.1.0
          Libs: -L\''${libdir} -lrust_accumulator -lws2_32 -luserenv -lbcrypt -lntdll
          Cflags: -I\''${includedir}
          EOF
        '';
      };
    };
}
