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
  peers = [
    "dan"
    "franco"
    "sasha"
    "sebastian"
  ];
  nodeId = "noon";

  # These three variables must agree
  nodeRelease = "preview";
  networkMagic = "2";
  # This is it's own var, as mithril calls "preprod" `release-preprod`
  # and "preview" `testing-preview`
  mithrilDir = "testing-${nodeRelease}";

  nodeVersion = "9.0.0"; # Note: This must match the node version in the flake.nix
in
{
  system.stateVersion = "24.05";

  # Incase we want to do some nixing
  nix.extraOptions = ''
    experimental-features = nix-command flakes ca-derivations
  '';

  services.openssh.enable = true;

  users.users.hydra = {
    isNormalUser = true;
    description = "hydra";
    extraGroups = [ "systemd-journal" ];
    initialPassword = ""; # No password

    # Your ssh key
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIATz/Jv+AnBft+9Q01UF07OydvgTTaTdCa+nMqabkUNl"
    ];
  };

  services.getty.autologinUser = "hydra";

  environment.systemPackages =
    let
      genHydraKey = pkgs.writeShellScriptBin "genHydraKey" ''
        set -e

        if [ -f ${nodeId}-hydra.sk ]; then
          echo "Found a hydra secret key for ${nodeId}; not generating another one."
          exit 0
        fi

        ${hydra.packages."${system}".hydra-node}/bin/hydra-node \
          gen-hydra-key \
            --output-file ${nodeId}-hydra
      '';
    in
    [
      # So you can just do:
      #
      #  > cd ~/cardano-data/credentials && genHydraKey
      #
      genHydraKey
      # These aren't really needed, as the systemd services just pull in the
      # binaries directly, but might be useful for debugging, so we leave them
      # in the system path.
      hydra.packages."${system}".hydra-node # To run a hydra node
      hydra.packages."${system}".hydra-tui # To interact with your node/peers
      cardano-node.packages."${system}".cardano-node # To talk to the cardano network
      mithril.packages."${system}".mithril-client-cli # Efficient syncing of the cardano node
      cardano-node.packages."${system}".cardano-cli # For any ad-hoc cardano actions we may like to run
    ];

  programs.bash.shellAliases = {
    # Run 'logs -f' to follow
    logs = "journalctl -u mithril-maybe-download -u cardano-node -u hydra-node -u necessary-files";
  };

  environment.variables = {
    "CARDANO_NODE_NETWORK_ID" = "${networkMagic}";
    "CARDANO_NODE_SOCKET_PATH" = "${cardanoDataPath}/node.socket";
  };

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
            necessaryFiles = pkgs.writeShellScriptBin "necessaryFiles" ''
              set -e

              if [ -d ${cardanoDataPath} ]; then
                echo "Not re-creating configs and credentials; ${cardanoDataPath} exists."
                exit 0
              fi

              mkdir -p ${cardanoDataPath}

              cd ${cardanoDataPath}

              # Get the node configs
              curl -L -O \
                https://github.com/IntersectMBO/cardano-node/releases/download/${nodeVersion}/cardano-node-${nodeVersion}-linux.tar.gz

              tar xf cardano-node-${nodeVersion}-linux.tar.gz \
                  ./share/${nodeRelease} \
                  --strip-components=3

              # Get our peer data (and peer config)
              git clone https://github.com/cardano-scaling/hydra-team-config.git

              # Jump to specific revision
              cd hydra-team-config && \
                git checkout dee7986b1377a0fa93d06cfc131ae7c26ca34299 && \
                cd ..

              # Make our cardano signing keys
              mkdir credentials
              cardano-cli address key-gen \
                --verification-key-file credentials/${nodeId}-node.vk \
                --signing-key-file credentials/${nodeId}-node.sk

              systemd-notify --ready
            '';
          in
          "${lib.getExe necessaryFiles}";
      };
    };


    mithril-maybe-download =
      let
      in
      {
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
            "AGGREGATOR_ENDPOINT=https://aggregator.${mithrilDir}.api.mithril.network/aggregator"
          ];
          # We need to wait a bit for the initial download.
          TimeoutStartSec = 10 * 60;
          ExecStart =
            let
              mithrilMaybeDownload = pkgs.writeShellScriptBin "mithrilMaybeDownload" ''
                set -e

                export GENESIS_VERIFICATION_KEY=''$(curl https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/${mithrilDir}/genesis.vkey 2> /dev/null)

                if [ ! -d db ]; then
                  ${mithril.packages.${system}.mithril-client-cli}/bin/mithril-client \
                    cardano-db \
                    download \
                    latest
                fi

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
                --socket-path ${cardanoDataPath}/node.socket \
                --database-path db
        '';
        Environment = [
          "CARDANO_NODE_NETWORK_ID=${networkMagic}"
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
          "CARDANO_NODE_NETWORK_ID=${networkMagic}"
          "CARDANO_NODE_SOCKET_PATH=${cardanoDataPath}/node.socket"
        ];
        # Wait 10 minutes before restarting
        RestartSec = 1 * 60;
        ExecStart =
          let
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
                peerArgs+=" --peer $(cat ''${folder}/''${peer}.peer) "
                peerArgs+=" --hydra-verification-key ''${folder}/''${peer}.hydra.vk "
                peerArgs+=" --cardano-verification-key ''${folder}/''${peer}.cardano.vk "
              done

              ${hydra.packages.${system}.hydra-node}/bin/hydra-node \
                --node-id ${nodeId} \
                --cardano-signing-key credentials/${nodeId}-node.sk \
                --hydra-signing-key credentials/${nodeId}-hydra.sk \
                --port ${port} \
                --api-host 0.0.0.0 \
                --host 0.0.0.0 \
                --testnet-magic ${networkMagic}  \
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
