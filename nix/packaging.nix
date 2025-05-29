# Vendored from https://github.com/input-output-hk/haskell-nix-example/blob/2247d445b91b0cc21eda4359e060d76b3cc0ce41/packaging.nix
#
# Modified to not be an overlay and changed the resultin zip format slightly.
{
  perSystem = { pkgs, ... }:
    {
      # Given one or more 'drvs', this produces a single zip containing all binaries
      # potentially patched for better distribution.
      _module.args.asZip = { name ? null }: drvs:
        let
          drv = if builtins.isList drvs then builtins.head drvs else drvs;
          name' = if name == null then drv.pname or drv.name else name;
          combined = pkgs.symlinkJoin { name = "${name'}-binaries"; paths = drvs; };
          inherit (drv.stdenv) targetPlatform;
          interpForSystem = sys:
            let
              s = {
                "i686-linux" = "/lib/ld-linux.so.2";
                "x86_64-linux" = "/lib64/ld-linux-x86-64.so.2";
                "aarch64-linux" = "/lib/ld-linux-aarch64.so.1";
                "armv7l-linux" = "/lib/ld-linux-armhf.so.3";
                "armv7a-linux" = "/lib/ld-linux-armhf.so.3";
              };
            in
              s.${sys} or (builtins.abort "Unsupported system ${sys}. Supported systms are: ${builtins.concatStringsSep ", " (builtins.attrNames s)}.");
          fixup-nix-deps = pkgs.writeShellApplication {
            name = "fixup-nix-deps";
            text = ''
              for nixlib in $(otool -L "$1" |awk '/nix\/store/{ print $1 }'); do
                  case "$nixlib" in
                  *libiconv.dylib) install_name_tool -change "$nixlib" /usr/lib/libiconv.dylib "$1" ;;
                  *libffi.*.dylib) install_name_tool -change "$nixlib" /usr/lib/libffi.dylib   "$1" ;;
                  *libc++.*.dylib) install_name_tool -change "$nixlib" /usr/lib/libc++.dylib   "$1" ;;
                  *libz.dylib)     install_name_tool -change "$nixlib" /usr/lib/libz.dylib     "$1" ;;
                  *) ;;
                  esac
              done
            '';
          };
        in
        pkgs.stdenv.mkDerivation {
          name = "${name'}.zip";
          buildInputs = with pkgs; [ patchelf zip fixup-nix-deps ];

          phases = [ "buildPhase" "installPhase" ];

          buildPhase = ''
            mkdir -p bin
            cp ${combined}/bin/* bin/
          ''
          # set the interpreter to the default expected location on linux. (See interpForSystem above)
          + pkgs.lib.optionalString (targetPlatform.isLinux && targetPlatform.isGnu) ''
            for bin in bin/*; do
              mode=$(stat -c%a $bin)
              chmod +w $bin
              patchelf --set-interpreter ${interpForSystem targetPlatform.system} $bin
              chmod $mode $bin
            done
          '' + pkgs.lib.optionalString targetPlatform.isDarwin ''
            for bin in bin/*; do
              mode=$(stat -c%a $bin)
              chmod +w $bin
              fixup-nix-deps $bin
              chmod $mode $bin
            done
          '';

          # compress and put into hydra products
          installPhase = ''
            mkdir -p $out/
            cd bin/
            zip -r -9 $out/${name'}.zip .
          '';

          passthru = {
            isPackage = true;
            packageName = "${name'}.zip";
          };
        };
    };
}
