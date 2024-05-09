{ compiler-nix-name

, inputMap

, pkgs

}:

pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
    name = "hydra";
    src = ./../..;
    filter = path: type:
      # Blacklist of paths which do not affect the haskell build. The smaller
      # the resulting list of files is, the less likely we have redundant
      # rebuilds.
      builtins.all (x: baseNameOf path != x) [
        "flake.nix"
        "flake.lock"
        "nix"
        ".github"
        "demo"
        "docs"
        "sample-node-config"
        "spec"
        "testnets"
      ];
  };
  projectFileName = "cabal.project";

  inherit inputMap compiler-nix-name;

  modules = [
    # Strip debugging symbols from exes (smaller closures)
    {
      packages.hydra-node.dontStrip = false;
      packages.hydra-tui.dontStrip = false;
      packages.hydraw.dontStrip = false;
    }
    # Use different static libs on darwin
    # TODO: Always use these?
    (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
      packages.hydra-node.ghcOptions = with pkgs; [
        "-L${lib.getLib static-gmp}/lib"
        "-L${lib.getLib static-libsodium-vrf}/lib"
        "-L${lib.getLib static-secp256k1}/lib"
        "-L${lib.getLib static-openssl}/lib"
        "-L${lib.getLib static-libblst}/lib"
      ];
    })
    {
      # lib:ghc is a bit annoying in that it comes with it's own build-type:Custom, and then tries
      # to call out to all kinds of silly tools that GHC doesn't really provide.
      # For this reason, we try to get away without re-installing lib:ghc for now.
      reinstallableLibGhc = false;
    }
    {
      # This is needed to stop GHC from trying to load libc++.a; I hope we'll
      # have a better fix for this eventually. For now this will have to do.
      packages.double-conversion.ghcOptions = [
        # stop putting U __gxx_personality_v0 into the library!
        "-optcxx-fno-rtti"
        "-optcxx-fno-exceptions"
        # stop putting U __cxa_guard_release into the library!
        "-optcxx-std=gnu++98"
        "-optcxx-fno-threadsafe-statics"
      ];
      # This is needed to stop failing when `git` can't be found. When
      # cross compiling, git does not exist in the target context, and thus can
      # not be executed.  Template Hsakell does not distinguish between native
      # and target.
      packages.gitrev.patches = [
        (builtins.toFile "gitrev.patch" ''
          diff --git a/src/Development/GitRev.hs b/src/Development/GitRev.hs
          index b664692..603ad1b 100644
          --- a/src/Development/GitRev.hs
          +++ b/src/Development/GitRev.hs
          @@ -62,7 +62,9 @@ runGit :: [String] -> String -> IndexUsed -> Q String
           runGit args def useIdx = do
             let oops :: SomeException -> IO (ExitCode, String, String)
                 oops _e = return (ExitFailure 1, def, "")
          +      none :: SomeException -> IO (Maybe FilePath)
          +      none _e = return Nothing
          -  gitFound <- runIO $ isJust <$> findExecutable "git"
          +  gitFound <- runIO $ isJust <$> findExecutable "git" `catch` none
             if gitFound
               then do
                 -- a lot of bookkeeping to record the right dependencies
        '')
      ];
    }
    # This is needed because plutus-tx, force loads the PlutusTx.Plugin, and then
    # we fail with
    #
    #   > <no location info>: error:
    #   >     Plugins require -fno-external-interpreter
    #
    # during the build.  Plugins and cross compilers are still WIP :-/

    (pkgs.lib.mkIf (pkgs.hostPlatform.isMusl && pkgs.hostPlatform.isAarch64) {
      packages.plutus-tx.patches = [
        (builtins.toFile "plutus-tx.patch" ''
          From 895a8a4af848ec29f9165fbff585f391d2c3358b Mon Sep 17 00:00:00 2001
          From: Moritz Angermann <moritz.angermann@gmail.com>
          Date: Sun, 10 Dec 2023 16:23:24 +0800
          Subject: [PATCH] Update TH.hs

          Just don't force load it.
          ---
          src/PlutusTx/TH.hs | 1 -
          1 file changed, 1 deletion(-)

          diff --git a/src/PlutusTx/TH.hs b/src/PlutusTx/TH.hs
          index 49f26f6585e..02a0f927dd5 100644
          --- a/src/PlutusTx/TH.hs
          +++ b/src/PlutusTx/TH.hs
          @@ -46,7 +46,6 @@ going to typecheck, and the result is always a 'CompiledCode', so that's also fi
           -- | Compile a quoted Haskell expression into a corresponding Plutus Core program.
           compileUntyped :: TH.Q TH.Exp -> TH.Q TH.Exp
           compileUntyped e = do
          -    TH.addCorePlugin "PlutusTx.Plugin"
               loc <- TH.location
               let locStr = TH.pprint loc
               -- See note [Typed TH]
        '')
      ];
    })
  ];
}
