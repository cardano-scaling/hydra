# A demo using process-compose
{ self, ... }:
{

  perSystem =
    { config
    , pkgs
    , pkgs-2411
    , self'
    , ...
    }:
    {
      process-compose."demo" = {
        package = pkgs.process-compose;
        settings = {
          log_location = "devnet/logs/process-compose.log";
          log_level = "debug";
          environment = {
            CARDANO_NODE_SOCKET_PATH = "devnet/node.socket";
            CARDANO_NODE_NETWORK_ID = "42";
          };

          processes = {
            prepare-devnet = {
              working_dir = ".";
              command = ''
                ${pkgs.bash}/bin/bash ${self}/demo/prepare-devnet.sh
              '';
            };
            cardano-node = {
              working_dir = ".";
              command = pkgs.writeShellApplication {
                name = "cardano-node";
                text = ''
                  ${pkgs.cardano-node}/bin/cardano-node run \
                    --config devnet/cardano-node.json \
                    --topology devnet/topology.json \
                    --database-path devnet/db \
                    --socket-path devnet/node.socket \
                    --shelley-operational-certificate devnet/opcert.cert \
                    --shelley-kes-key devnet/kes.skey \
                    --shelley-vrf-key devnet/vrf.skey
                '';
              };
              ready_log_line = "NodeIsLeader";
              depends_on."prepare-devnet".condition = "process_completed";
            };
            seed-devnet = {
              working_dir = ".";
              command = ''
                ${self}/demo/seed-devnet.sh ${pkgs.cardano-cli}/bin/cardano-cli ${self'.packages.hydra-node}/bin/hydra-node
              '';
              depends_on."cardano-node".condition = "process_log_ready";
            };
            hydra-node-alice = {
              log_location = "./devnet/alice-logs.txt";
              command = pkgs.writeShellApplication {
                name = "hydra-node-alice";
                checkPhase = ""; # not shellcheck and choke on sourcing .env
                runtimeInputs = [ pkgs.gettext ];
                text = ''
                  set -a; [ -f .env ] && source .env; set +a
                  ${self'.packages.hydra-node}/bin/hydra-node \
                    --config demo/configs/alice.yaml \
                    --hydra-scripts-tx-id ''$HYDRA_SCRIPTS_TX_ID
                '';
              };
              working_dir = ".";
              ready_log_line = "NodeIsLeader";
              depends_on."seed-devnet".condition = "process_completed";
            };
            hydra-node-bob = {
              log_location = "./devnet/bob-logs.txt";
              command = pkgs.writeShellApplication {
                name = "hydra-node-bob";
                checkPhase = ""; # not shellcheck and choke on sourcing .env
                runtimeInputs = [ pkgs.gettext ];
                text = ''
                  set -a; [ -f .env ] && source .env; set +a
                  ${self'.packages.hydra-node}/bin/hydra-node \
                    --config demo/configs/bob.yaml \
                    --hydra-scripts-tx-id ''$HYDRA_SCRIPTS_TX_ID
                '';
              };
              working_dir = ".";
              ready_log_line = "NodeIsLeader";
              depends_on."seed-devnet".condition = "process_completed";
            };
            hydra-node-carol = {
              log_location = "./devnet/carol-logs.txt";
              command = pkgs.writeShellApplication {
                name = "hydra-node-carol";
                checkPhase = ""; # not shellcheck and choke on sourcing .env
                runtimeInputs = [ pkgs.gettext ];
                text = ''
                  set -a; [ -f .env ] && source .env; set +a
                  ${self'.packages.hydra-node}/bin/hydra-node \
                    --config demo/configs/carol.yaml \
                    --hydra-scripts-tx-id ''$HYDRA_SCRIPTS_TX_ID
                '';
              };
              working_dir = ".";
              ready_log_line = "NodeIsLeader";
              depends_on."seed-devnet".condition = "process_completed";
            };
            hydra-tui-alice = {
              working_dir = "./demo";
              command = pkgs.writeShellApplication {
                name = "alice-tui";
                text = ''
                  ${self'.packages.hydra-tui}/bin/hydra-tui \
                    --connect 0.0.0.0:4001 \
                    --node-socket devnet/node.socket \
                    --testnet-magic 42 \
                    --cardano-signing-key "${config.hydra.demo.fixtures.parties.alice.cardano.funds.sk}" \
                    --fuel-key "${config.hydra.demo.fixtures.parties.alice.cardano.fuel.vk}"
                '';
              };
              is_foreground = true;
              depends_on."hydra-node-alice".condition = "process_started";
            };
            hydra-tui-bob = {
              working_dir = "./demo";
              command = pkgs.writeShellApplication {
                name = "bob-tui";
                text = ''
                  ${self'.packages.hydra-tui}/bin/hydra-tui \
                  --connect 0.0.0.0:4002 \
                  --node-socket devnet/node.socket \
                  --testnet-magic 42 \
                  --cardano-signing-key "${config.hydra.demo.fixtures.parties.bob.cardano.funds.sk}" \
                  --fuel-key "${config.hydra.demo.fixtures.parties.bob.cardano.fuel.vk}"
                '';
              };
              is_foreground = true;
              depends_on."hydra-node-bob".condition = "process_started";
            };
            hydra-tui-carol = {
              working_dir = "./demo";
              command = pkgs.writeShellApplication {
                name = "carol";
                text = ''
                  ${self'.packages.hydra-tui}/bin/hydra-tui \
                    --connect 0.0.0.0:4003 \
                    --node-socket devnet/node.socket \
                    --testnet-magic 42 \
                    --cardano-signing-key "${config.hydra.demo.fixtures.parties.carol.cardano.funds.sk}" \
                    --fuel-key "${config.hydra.demo.fixtures.parties.carol.cardano.fuel.vk}"
                '';
              };
              is_foreground = true;
              depends_on."hydra-node-carol".condition = "process_started";
            };
            prometheus = {
              working_dir = "./demo";
              command = pkgs.writeShellApplication {
                name = "prometheus";
                text = ''
                  ${pkgs.prometheus}/bin/prometheus \
                      --config.file=./prometheus/nix/prometheus.yml \
                      --storage.tsdb.path=./devnet/prometheus \
                      --web.listen-address=127.0.0.1:9090
                '';
              };
              depends_on = {
                "hydra-node-alice".condition = "process_started";
                "hydra-node-bob".condition = "process_started";
                "hydra-node-carol".condition = "process_started";
              };
            };
            grafana = {
              working_dir = "./demo";
              command = pkgs.writeShellApplication {
                name = "grafana";
                text = ''
                  ${pkgs-2411.grafana}/bin/grafana server \
                      --homepath ${pkgs-2411.grafana}/share/grafana \
                      cfg:default.paths.data="$(pwd)/devnet/grafana/data" \
                      cfg:default.paths.logs="$(pwd)/devnet/grafana/logs" \
                      cfg:default.paths.plugins="$(pwd)/devnet/grafana/plugins" \
                      cfg:default.paths.provisioning="$(pwd)/grafana/nix"
                '';
              };
              depends_on."prometheus".condition = "process_started";
            };
          };
        };
      };
    };
}
