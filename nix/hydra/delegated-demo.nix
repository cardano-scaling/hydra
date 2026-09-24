# A "delegated" demo using process-compose: alice/bob/carol run hydra-nodes as
# pure mediators (they hold Hydra keys and pay for head chain txs but own none of
# the head's funds), while two people who are not part of the network, Anna and
# Elsa, bring their own funds in, transact on L2, and withdraw to L1.
#
# See nix/hydra/demo.nix for the sibling operator-owned-funds demo.
{ self, ... }:
{

  perSystem =
    { pkgs
    , self'
    , ...
    }:
    let
      inherit (pkgs) lib;

      # Tools the bash/websocat drivers need on PATH.
      driverInputs = [
        pkgs.cardano-cli
        pkgs.websocat
        pkgs.jq
        pkgs.curl
        pkgs.coreutils
        pkgs.gnused
        pkgs.gawk
        self'.packages.hydra-node
      ];

      # A mediator hydra-node, wired with flat flags (matching run-tmux.py).
      # deposit-period is high (500s): the node sizes a deposit tx's validity
      # window at min(maxGraceTime = 200, deposit-period / 2) slots, and on this
      # 0.1s-slot devnet a small value leaves under a second to sign and submit
      # the external commit with cardano-cli. 500s pins it to the ~20s cap.
      mkNode = { id, name, api, listen, mon, peers, others }:
        pkgs.writeShellApplication {
          name = "hydra-node-${name}";
          checkPhase = ""; # do not shellcheck: it chokes on sourcing .env
          text = ''
            set -a; [ -f .env ] && source .env; set +a
            exec ${self'.packages.hydra-node}/bin/hydra-node \
              --node-id ${toString id} \
              --listen 127.0.0.1:${toString listen} \
              --api-port ${toString api} \
              --monitoring-port ${toString mon} \
              ${lib.concatMapStringsSep " " (p: "--peer 127.0.0.1:${toString p}") peers} \
              --hydra-signing-key demo/${name}.sk \
              ${lib.concatMapStringsSep " " (o: "--hydra-verification-key demo/${o}.vk") others} \
              --hydra-scripts-tx-id ''$HYDRA_SCRIPTS_TX_ID \
              --cardano-signing-key devnet/credentials/${name}.sk \
              ${lib.concatMapStringsSep " " (o: "--cardano-verification-key devnet/credentials/${o}.vk") others} \
              --ledger-protocol-parameters devnet/protocol-parameters.json \
              --persistence-dir devnet/persistence/${name} \
              --contestation-period 3s \
              --deposit-period 500s \
              --deposit-activation 10s \
              --testnet-magic 42 \
              --node-socket devnet/node.socket
          '';
        };

      # Wrap one of the delegated-demo shell scripts with the driver tools on PATH.
      mkDriver = drvName: scriptArgs:
        pkgs.writeShellApplication {
          name = drvName;
          runtimeInputs = driverInputs;
          text = "exec ${pkgs.bash}/bin/bash ${self}/delegated-demo/${scriptArgs}";
        };

    in
    {
      process-compose."delegated-demo" = {
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
              command = mkDriver "seed-delegated-devnet" "seed-devnet.sh";
              depends_on."cardano-node".condition = "process_log_ready";
            };
            hydra-node-alice = {
              working_dir = ".";
              log_location = "./devnet/alice-logs.txt";
              command = mkNode { id = 1; name = "alice"; api = 4001; listen = 5001; mon = 6001; peers = [ 5002 5003 ]; others = [ "bob" "carol" ]; };
              ready_log_line = "NodeSynced";
              depends_on."seed-devnet".condition = "process_completed";
            };
            hydra-node-bob = {
              working_dir = ".";
              log_location = "./devnet/bob-logs.txt";
              command = mkNode { id = 2; name = "bob"; api = 4002; listen = 5002; mon = 6002; peers = [ 5001 5003 ]; others = [ "alice" "carol" ]; };
              ready_log_line = "NodeSynced";
              depends_on."seed-devnet".condition = "process_completed";
            };
            hydra-node-carol = {
              working_dir = ".";
              log_location = "./devnet/carol-logs.txt";
              command = mkNode { id = 3; name = "carol"; api = 4003; listen = 5003; mon = 6003; peers = [ 5001 5002 ]; others = [ "alice" "bob" ]; };
              ready_log_line = "NodeSynced";
              depends_on."seed-devnet".condition = "process_completed";
            };
            actor-anna = {
              working_dir = ".";
              command = mkDriver "actor-anna" "actor.sh anna 4001";
              is_foreground = true;
              depends_on."hydra-node-alice".condition = "process_log_ready";
            };
            actor-elsa = {
              working_dir = ".";
              command = mkDriver "actor-elsa" "actor.sh elsa 4002";
              is_foreground = true;
              depends_on."hydra-node-bob".condition = "process_log_ready";
            };
            l1-observer = {
              working_dir = ".";
              command = mkDriver "l1-observer" "observe.sh";
              depends_on = {
                "hydra-node-alice".condition = "process_started";
                "hydra-node-bob".condition = "process_started";
              };
            };
            # Operator's view of the head, via the pure mediator carol (:4003).
            # carol owns no head funds, so the Funds tab shows only her node fuel;
            # the head status, parties and UTxO set are the shared head state.
            hydra-tui-operator = {
              working_dir = ".";
              command = pkgs.writeShellApplication {
                name = "operator-tui";
                text = ''
                  ${self'.packages.hydra-tui}/bin/hydra-tui \
                    --connect 0.0.0.0:4003 \
                    --node-socket devnet/node.socket \
                    --testnet-magic 42 \
                    --funds-signing-key devnet/credentials/carol.sk \
                    --fuel-key devnet/credentials/carol.vk
                '';
              };
              is_foreground = true;
              depends_on."hydra-node-carol".condition = "process_log_ready";
            };
          };
        };
      };
    };
}
