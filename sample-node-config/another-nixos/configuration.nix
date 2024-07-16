{ config
, pkgs
, lib
, system
, cardano-node
, hydra
, mithril
, ...
}:

let
  cardanoDataPath = "/home/hydra/cardano-data";
  nodeRelease = "preprod";
  peers = [ "franco" "sasha" "sebastian" "dan" ];
  nodeId = "noon";
in
{
  system.stateVersion = "24.05";

  # For host keys; see:
  # <https://fzakaria.com/2024/07/12/nix-secrets-for-dummies.html>
  services.openssh.enable = true;

  users.users.hydra = {
    isNormalUser = true;
    description = "hydra";
    extraGroups = [ "systemd-journal" ];
    initialPassword = ""; # No password
  };

  services.getty.autologinUser = "hydra";

  environment.systemPackages =
    [
      # These aren't really needed, as the systemd services just pull in the
      # binaries directly, but might be useful for debugging, so we leave them
      # in the system path.
      hydra.packages."${system}".hydra-node # To run a hydra node
      cardano-node.packages."${system}".cardano-node # To talk to the cardano network
      mithril.packages."${system}".mithril-client-cli # Efficient syncing of the cardano node
      hydra.packages."${system}".hydraw # Hydra drawing game
      cardano-node.packages."${system}".cardano-cli # For any ad-hoc cardano actions we may like to run
    ];

  programs.bash.shellAliases = {
    # Run 'logs -f' to follow
    logs = "journalctl -u mithril-maybe-download -u cardano-node -u hydra-node -u necessary-files";
  };

  # TODO: Add them in for ad-hoc stuff?
  # environment.variables = {
  # };
  #

  systemd.services = {
    necessary-files = {
      requires = [ "network-online.target" ];
      after = [ "network-online.target" ];
      wantedBy = [ "mithril-maybe-download.target" ];
      path = with pkgs; [
        curl
        gnutar
        gzip
        git
        cardano-node.packages."${system}".cardano-cli
      ];
      serviceConfig = {
        User = "hydra";
        Type = "notify";
        NotifyAccess = "all";
        ExecStart =
          let
            nodeVersion = "8.9.3";
            necessaryFiles = pkgs.writeShellScriptBin "necessaryFiles" ''
              set -e
              mkdir -p ${cardanoDataPath}

              cd ${cardanoDataPath}

              # Get the node configs
              curl -L -O \
                https://github.com/IntersectMBO/cardano-node/releases/download/${nodeVersion}/cardano-node-${nodeVersion}-linux.tar.gz

              tar xf cardano-node-${nodeVersion}-linux.tar.gz \
                  ./share/${nodeRelease} \
                  --strip-components=3

              # Get our peer data
              git clone https://github.com/cardano-scaling/hydra-team-config.git

              # Make our cardano signing keys, if they don't already exist
              if [ ! -d credentials ]; then
                mkdir credentials
                cardano-cli address key-gen \
                  --verification-key-file credentials/${nodeId}-node.vk \
                  --signing-key-file credentials/${nodeId}.sk
              fi

              systemd-notify --ready
            '';
          in
          "${lib.getExe necessaryFiles}";
      };
    };


    mithril-maybe-download = {
      requires = [ "network-online.target" "necessary-files.service" ];
      after = [ "necessary-files.service" ];
      wantedBy = [ "multi-user.target" ];
      path = with pkgs; [ curl ];
      serviceConfig = {
        Type = "notify";
        NotifyAccess = "all";
        User = "hydra";
        WorkingDirectory = cardanoDataPath;
        Environment = [
          "AGGREGATOR_ENDPOINT=https://aggregator.release-${nodeRelease}.api.mithril.network/aggregator"
        ];
        # We need to wait a bit for the initial download.
        TimeoutStartSec = 10 * 60;
        ExecStart =
          let
            mithrilMaybeDownload = pkgs.writeShellScriptBin "mithrilMaybeDownload" ''
              set -e

              export GENESIS_VERIFICATION_KEY=''$(curl https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-${nodeRelease}/genesis.vkey 2> /dev/null)

              # if [ ! -d db ]; then
              #   ${mithril.packages.${system}.mithril-client-cli}/bin/mithril-client \
              #     cardano-db \
              #     download \
              #     latest
              # fi

              systemd-notify --ready
            '';
          in
          "${lib.getExe mithrilMaybeDownload}";
      };
    };


    cardano-node = {
      requires = [ "mithril-maybe-download.service" "necessary-files.service" ];
      after = [ "mithril-maybe-download.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        User = "hydra";
        WorkingDirectory = cardanoDataPath;
        ExecStart = ''${lib.getExe cardano-node.packages.${system}.cardano-node} \
                run \
                --config config.json \
                --topology topology.json \
                --database-path db
        '';
        Environment = [
          "CARDANO_NODE_NETWORK_ID=1"
          "CARDANO_NODE_SOCKET_PATH=${cardanoDataPath}/node.socket"
        ];
      };
    };


    hydra-node = {
      after = [ "cardano-node.service" ];
      requires = [ "cardano-node.service" ];
      wantedBy = [ "multi-user.target" ];
      path = with pkgs; [ git ];
      serviceConfig = {
        User = "hydra";
        WorkingDirectory = cardanoDataPath;
        Environment = [
          "CARDANO_NODE_NETWORK_ID=1"
        ];
        ExecStart =
          let
            # TODO: My key
            hydra-signing-key = "";
            port = "5005";
            # Select only the friends we want from the full list:
            # <https://github.com/input-output-hk/hydra-team-config/tree/master/parties>
            # We build a bash list out of a nix list :tear:
            peerList = pkgs.lib.strings.concatMapStringsSep " " (x: "\"${x}\"") peers;
            spinupHydra = pkgs.writeShellScriptBin "spinupHydra" ''
              set -e

              peerArgs="";
              folder=hydra-team-config/parties
              for peer in ${peerList};
              do
                peerArgs+=" --peer $(cat ''${folder}/''${peer}.peer)"
                peerArgs+=" --hydra-verification-key ''${folder}/''${peer}.hydra.vk"
                peerArgs+=" --cardano-verification-key ''${folder}/''${peer}.cardano.vk"
              done

              echo "Peer args=''${peerArgs}"

              ${hydra.packages.${system}.hydra-node}/bin/hydra-node \
                --node-id ${nodeId} \
                --cardano-signing-key credentials/${nodeId}.node.sk \
                --hydra-signing-key parties/${nodeId}.hydra.sk \
                --port ${port} \
                --api-host 0.0.0.0 \
                --host 0.0.0.0 \
                --testnet-magic ''${CARDANO_NODE_NETWORK_ID} \
                --node-socket node.socket \
                --persistence-dir persistence \
                --ledger-protocol-parameters hydra-team-config/protocol-parameters.json \
                --hydra-scripts-tx-id $(cat hydra-team-config/hydra-scripts-tx-id) \
                ''${peerArgs}
            '';
          in
          "${lib.getExe spinupHydra}";

        Restart = "on-failure";
      };
    };
  };
}
