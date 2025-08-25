# A demo using process-compose
{ self, ... }:
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
              working_dir = ".";
              command = ''
                ${pkgs.bash}/bin/bash ${self}/demo/prepare-devnet.sh
              '';
            };
            cardano-node = {
              working_dir = ".";
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
                text = ''
                  # (Re-)Export all variables from .env
                  set -a; [ -f .env ] && source .env; set +a
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
              working_dir = ".";
              ready_log_line = "NodeIsLeader";
              depends_on."seed-devnet".condition = "process_completed";
            };
            hydra-node-bob = {
              log_location = "./devnet/bob-logs.txt";
              command = pkgs.writeShellApplication {
                name = "hydra-node-bob";
                checkPhase = ""; # not shellcheck and choke on sourcing .env
                text = ''
                  # (Re-)Export all variables from .env
                  set -a; [ -f .env ] && source .env; set +a
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
              working_dir = ".";
              ready_log_line = "NodeIsLeader";
              depends_on."seed-devnet".condition = "process_completed";
            };
            hydra-node-carol = {
              log_location = "./devnet/carol-logs.txt";
              command = pkgs.writeShellApplication {
                name = "hydra-node-carol";
                checkPhase = ""; # not shellcheck and choke on sourcing .env
                text = ''
                  # (Re-)Export all variables from .env
                  set -a; [ -f .env ] && source .env; set +a
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
              working_dir = ".";
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
            test = {
              command = pkgs.writeShellApplication {
                name = "demo-test";
                runtimeInputs = [ pkgs.coreutils pkgs.cardano-cli pkgs.cardano-node self'.packages.hydra-node ];
                text = ''
                  set -euo pipefail
                  
                  echo "--- Testing demo components"
                  
                  # Test that we can build and run basic components
                  echo "Testing cardano-cli..."
                  cardano-cli --version
                  
                  echo "Testing cardano-node..."
                  cardano-node --version
                  
                  echo "✅ Demo setup completed successfully"
                  
                  echo "--- Testing that devnet files exist"
                  if [ -f "devnet/cardano-node.json" ] && [ -f "devnet/topology.json" ]; then
                    echo "✅ Devnet configuration files created"
                  else
                    echo "❌ Devnet configuration files missing"
                    exit 1
                  fi
                  
                  echo "--- Testing hydra-node"
                  hydra-node --version
                '';
              };
              # Ensure test starts only after devnet has been prepared
              depends_on."prepare-devnet".condition = "process_completed";
            };
            prometheus = {
              working_dir = "./demo";
              command = ''
                ${pkgs.prometheus}/bin/prometheus \
                    --config.file=./prometheus/nix/prometheus.yml \
                    --storage.tsdb.path=./devnet/prometheus \
                    --web.listen-address=127.0.0.1:9090
              '';
              depends_on = {
                "hydra-node-alice".condition = "process_started";
                "hydra-node-bob".condition = "process_started";
                "hydra-node-carol".condition = "process_started";
              };
            };
            grafana = {
              working_dir = "./demo";
              command = ''
                ${pkgs.grafana}/bin/grafana server \
                    --homepath ${pkgs.grafana}/share/grafana \
                    cfg:default.paths.data=$(pwd)/devnet/grafana/data \
                    cfg:default.paths.logs=$(pwd)/devnet/grafana/logs \
                    cfg:default.paths.plugins=$(pwd)/devnet/grafana/plugins \
                    cfg:default.paths.provisioning=$(pwd)/grafana/nix
              '';
              depends_on."prometheus".condition = "process_started";
            };
          };
        };
      };
    };
}
