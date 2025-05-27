# A demo using process-compose
{ inputs, self, ... }:
{

  perSystem = { pkgs, self', ... }:
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
              command = "${self}/demo/prepare-devnet.sh";
            };
            cardano-node = {
              command = ''
                ${pkgs.cardano-node}/bin/cardano-node run \
                --config devnet/cardano-node.json \
                --topology devnet/topology.json \
                --database-path devnet/db \
                --socket-path devnet/node.socket \
                --shelley-operational-certificate devnet/opcert.cert \
                --shelley-kes-key devnet/kes.skey \
                --shelley-vrf-key devnet/vrf.skey
              '';
              ready_log_line = "NodeIsLeader";
              depends_on."prepare-devnet".condition = "process_completed";
            };
            seed-devnet = {
              command = ''
                ${self}/demo/seed-devnet.sh ${pkgs.cardano-cli}/bin/cardano-cli ${self'.packages.hydra-node}/bin/hydra-node
              '';
              depends_on."cardano-node".condition = "process_log_ready";
            };
            hydra-node-alice = {
              command = pkgs.writeShellApplication {
                name = "hydra-node-alice";
                checkPhase = ""; # not shellcheck and choke on sourcing .env
                text = ''
                  # (Re-)Export all variables from .env
                  set -a; source .env; set +a
                  ${self'.packages.hydra-node}/bin/hydra-node \
                    --node-id 1 \
                    --listen 127.0.0.1:5001 \
                    --api-port 4001 \
                    --monitoring-port 6001 \
                    --peer 127.0.0.1:5002 \
                    --peer 127.0.0.1:5003 \
                    --hydra-signing-key ${self}/demo/alice.sk \
                    --hydra-verification-key ${self}/demo/bob.vk \
                    --hydra-verification-key ${self}/demo/carol.vk \
                    --hydra-scripts-tx-id ''$HYDRA_SCRIPTS_TX_ID \
                    --cardano-signing-key devnet/credentials/alice.sk \
                    --cardano-verification-key devnet/credentials/bob.vk \
                    --cardano-verification-key devnet/credentials/carol.vk \
                    --ledger-protocol-parameters devnet/protocol-parameters.json \
                    --testnet-magic 42 \
                    --node-socket devnet/node.socket \
                    --persistence-dir devnet/persistence/alice \
                    --contestation-period 3s
                '';
              };
              ready_log_line = "NodeIsLeader";
              depends_on."seed-devnet".condition = "process_completed";
            };
            hydra-node-bob = {
              command = pkgs.writeShellApplication {
                name = "hydra-node-bob";
                checkPhase = ""; # not shellcheck and choke on sourcing .env
                text = ''
                  # (Re-)Export all variables from .env
                  set -a; source .env; set +a
                  ${self'.packages.hydra-node}/bin/hydra-node \
                  --node-id 2 \
                  --listen 127.0.0.1:5002 \
                  --api-port 4002 \
                  --monitoring-port 6002 \
                  --peer 127.0.0.1:5001 \
                  --peer 127.0.0.1:5003 \
                  --hydra-signing-key ${self}/demo/bob.sk \
                  --hydra-verification-key ${self}/demo/alice.vk \
                  --hydra-verification-key ${self}/demo/carol.vk \
                  --hydra-scripts-tx-id ''$HYDRA_SCRIPTS_TX_ID \
                  --cardano-signing-key devnet/credentials/bob.sk \
                  --cardano-verification-key devnet/credentials/alice.vk \
                  --cardano-verification-key devnet/credentials/carol.vk \
                  --ledger-protocol-parameters devnet/protocol-parameters.json \
                  --testnet-magic 42 \
                  --node-socket devnet/node.socket \
                  --persistence-dir devnet/persistence/bob \
                  --contestation-period 3s
                '';
              };
              ready_log_line = "NodeIsLeader";
              depends_on."seed-devnet".condition = "process_completed";
            };
            hydra-node-carol = {
              command = pkgs.writeShellApplication {
                name = "hydra-node-carol";
                checkPhase = ""; # not shellcheck and choke on sourcing .env
                text = ''
                  # (Re-)Export all variables from .env
                  set -a; source .env; set +a
                  ${self'.packages.hydra-node}/bin/hydra-node \
                  --node-id 3 \
                  --listen 127.0.0.1:5003 \
                  --api-port 4003 \
                  --monitoring-port 6003 \
                  --peer 127.0.0.1:5001 \
                  --peer 127.0.0.1:5002 \
                  --hydra-signing-key ${self}/demo/carol.sk \
                  --hydra-verification-key ${self}/demo/alice.vk \
                  --hydra-verification-key ${self}/demo/bob.vk \
                  --hydra-scripts-tx-id ''$HYDRA_SCRIPTS_TX_ID \
                  --cardano-signing-key devnet/credentials/carol.sk \
                  --cardano-verification-key devnet/credentials/alice.vk \
                  --cardano-verification-key devnet/credentials/bob.vk \
                  --ledger-protocol-parameters devnet/protocol-parameters.json \
                  --testnet-magic 42 \
                  --node-socket devnet/node.socket \
                  --persistence-dir devnet/persistence/carol \
                  --contestation-period 3s
                '';
              };
              ready_log_line = "NodeIsLeader";
              depends_on."seed-devnet".condition = "process_completed";
            };
            hydra-tui-alice = {
              working_dir = "./demo";
              command = ''
                ${self'.packages.hydra-tui}/bin/hydra-tui \
                  --connect 0.0.0.0:4001 \
                  --node-socket devnet/node.socket \
                  --testnet-magic 42 \
                  --cardano-signing-key devnet/credentials/alice-funds.sk
              '';
              is_foreground = true;
              depends_on."hydra-node-alice".condition = "process_started";
            };
            hydra-tui-bob = {
              working_dir = "./demo";
              command = ''
                ${self'.packages.hydra-tui}/bin/hydra-tui \
                  --connect 0.0.0.0:4002 \
                  --node-socket devnet/node.socket \
                  --testnet-magic 42 \
                  --cardano-signing-key devnet/credentials/bob-funds.sk
              '';
              is_foreground = true;
              depends_on."hydra-node-bob".condition = "process_started";
            };
            hydra-tui-carol = {
              working_dir = "./demo";
              command = ''
                ${self'.packages.hydra-tui}/bin/hydra-tui \
                  --connect 0.0.0.0:4003 \
                  --node-socket devnet/node.socket \
                  --testnet-magic 42 \
                  --cardano-signing-key devnet/credentials/carol-funds.sk
              '';
              is_foreground = true;
              depends_on."hydra-node-carol".condition = "process_started";
            };
          };
        };
      };
    };
}
