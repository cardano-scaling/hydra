local:

let
  inherit (local.flake-parts-lib)
    mkPerSystemOption;
  inherit (local.lib)
    mkOption
    types;
  keyPair = types.submodule {
    options = {
      sk = mkOption {
        type = types.path;
      };
      vk = mkOption {
        type = types.path;
      };
    };
  };
  party = types.submodule {
    options = {
      cardano = {
        fuel = mkOption {
          type = keyPair;
        };
        funds = mkOption {
          type = keyPair;
        };
      };
      hydra = mkOption {
        type = keyPair;
      };
    };
  };
in
{

  imports = [
    (local.flake-parts-lib.importAndPublish
      "hydra-demo-fixtures"
      (_caller: {

        options.perSystem = mkPerSystemOption ({ lib, ... }: {
          options.hydra.demo.fixtures.parties = mkOption {
            type = types.attrsOf party;
            default = lib.genAttrs [ "alice" "bob" "carol" ] (name: {
              cardano = {
                fuel = {
                  sk = "${local.self}/hydra-cluster/config/credentials/${name}.sk";
                  vk = "${local.self}/hydra-cluster/config/credentials/${name}.vk";
                };
                funds = {
                  sk = "${local.self}/hydra-cluster/config/credentials/${name}-funds.sk";
                  vk = lib.cleanSource "${local.self}/hydra-cluster/config/credentials/${name}-funds.vk";
                };
              };
              hydra = {
                sk = "${local.self}/demo/${name}.sk";
                vk = "${local.self}/demo/${name}.vk";
              };
            });
          };
        });
      }))
  ];
}

