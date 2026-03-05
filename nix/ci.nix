{ config
, lib
, withSystem
, ...
}: {
  flake.hydraJobs = lib.genAttrs config.systems (lib.flip withSystem (
    { config
    , pkgs
    , ...
    }:
    let
      jobs = { inherit (config) packages checks devShells; };
    in
    jobs
    // {
      required = pkgs.releaseTools.aggregate {
        name = "required";
        constituents = lib.collect lib.isDerivation jobs;
      };
    }
  ));
}

