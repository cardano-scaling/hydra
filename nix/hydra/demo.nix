# A demo using process-compose

{ system ? builtins.currentSystem
, pkgs
, inputs
, cardano-cli
, cardano-node
, hydra-node
, self
, demoDir
, process-compose
}: {
  # httpServer.enable = true;
  package = process-compose;
  settings = {
    log_location = "./devnet/logs/all.log";
    log_level = "debug";
    environment = {
      BASEDIR = "./";
      SCRIPT_DIR = "./";
      CARDANO_NODE_SOCKET_PATH = "devnet/node.socket";
      CARDANO_NODE_NETWORK_ID = "42";
    };

    processes = {
      prepare-devnet = {
        command = "${demoDir}/prepare-devnet.sh";
      };
      cardano-node = {
        command = ''
          ${cardano-node}/bin/cardano-node run \
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
        command = "${demoDir}/seed-devnet.sh ${cardano-cli}/bin/cardano-cli ${hydra-node}/bin/hydra-node";
        depends_on."cardano-node".condition = "process_log_ready";
      };
      hydra-node-alice = {
        command = ''
          source .env &&
          ${hydra-node}/bin/hydra-node \
          --node-id 1 \
          --port 5001 \
          --api-port 4001 \
          --monitoring-port 6001 \
          --peer 127.0.0.1:5002 \
          --peer 127.0.0.1:5003 \
          --hydra-signing-key ${demoDir}/alice.sk \
          --hydra-verification-key ${demoDir}/bob.vk \
          --hydra-verification-key ${demoDir}/carol.vk \
          --hydra-scripts-tx-id  $HYDRA_SCRIPTS_TX_ID \
          --cardano-signing-key devnet/credentials/alice.sk \
          --cardano-verification-key devnet/credentials/bob.vk \
          --cardano-verification-key devnet/credentials/carol.vk \
          --ledger-protocol-parameters devnet/protocol-parameters.json \
          --testnet-magic 42 \
          --node-socket devnet/node.socket \
          --persistence-dir devnet
        '';
        ready_log_line = "NodeIsLeader";
        depends_on."seed-devnet".condition = "process_completed";
      };
      hydra-node-bob = {
        command = ''
          source .env &&
          ${hydra-node}/bin/hydra-node \
          --node-id 2 \
          --port 5002 \
          --api-port 4002 \
          --monitoring-port 6002 \
          --peer 127.0.0.1:5001 \
          --peer 127.0.0.1:5003 \
          --hydra-signing-key ${demoDir}/bob.sk \
          --hydra-verification-key ${demoDir}/alice.vk \
          --hydra-verification-key ${demoDir}/carol.vk \
          --hydra-scripts-tx-id  $HYDRA_SCRIPTS_TX_ID \
          --cardano-signing-key devnet/credentials/bob.sk \
          --cardano-verification-key devnet/credentials/alice.vk \
          --cardano-verification-key devnet/credentials/carol.vk \
          --ledger-protocol-parameters devnet/protocol-parameters.json \
          --testnet-magic 42 \
          --node-socket devnet/node.socket \
          --persistence-dir devnet
        '';
        ready_log_line = "NodeIsLeader";
        depends_on."seed-devnet".condition = "process_completed";
      };
      hydra-node-carol = {
        command = ''
          source .env &&
          ${hydra-node}/bin/hydra-node \
          --node-id 3 \
          --port 5003 \
          --api-port 4003 \
          --monitoring-port 6003 \
          --peer 127.0.0.1:5001 \
          --peer 127.0.0.1:5002 \
          --hydra-signing-key ${demoDir}/carol.sk \
          --hydra-verification-key ${demoDir}/alice.vk \
          --hydra-verification-key ${demoDir}/bob.vk \
          --hydra-scripts-tx-id  $HYDRA_SCRIPTS_TX_ID \
          --cardano-signing-key devnet/credentials/carol.sk \
          --cardano-verification-key devnet/credentials/alice.vk \
          --cardano-verification-key devnet/credentials/bob.vk \
          --ledger-protocol-parameters devnet/protocol-parameters.json \
          --testnet-magic 42 \
          --node-socket devnet/node.socket \
          --persistence-dir devnet
        '';
        ready_log_line = "NodeIsLeader";
        depends_on."seed-devnet".condition = "process_completed";
      };
    };
  };
}
