{ pkgs
, lib
, system
, cardano-node
, hydra
, mithril
, ...
}:

let
  cardanoDataPath = "/home/hydra/cardano-data";

  # Select only the friends we want from the full list:
  # <https://github.com/input-output-hk/hydra-team-config/tree/master/parties>
  peers = [
    "dan"
    "franco"
    "sasha"
    "sebastian"
  ];

  nodeId = "noon";
  hydraPort = "5005";

  # These three variables must agree
  networkName = "preview";
  networkMagic = "2";
  # This is it's own var, as mithril calls "preprod" `release-preprod`
  # and "preview" `testing-preview`
  mithrilDir = "testing-${networkName}";

  nodeVersion = "9.0.0"; # Note: This must match the node version in the flake.nix

  commonEnvVars = {
    "CARDANO_NODE_NETWORK_ID" = "${networkMagic}";
    "CARDANO_NODE_SOCKET_PATH" = "${cardanoDataPath}/node.socket";
  };

in
{
  system.stateVersion = "24.05";

  # Incase we want to do some nixing
  nix = {
    settings.trusted-users = [ "root" "hydra" ];
    extraOptions = ''
      experimental-features = nix-command flakes ca-derivations
    '';
  };

  services.openssh = {
    settings.PasswordAuthentication = false;
    enable = true;
  };

  users.users.hydra = {
    isNormalUser = true;
    description = "hydra";
    extraGroups = [ "systemd-journal" "wheel" ];
    initialPassword = ""; # No password

    # Your ssh key
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIATz/Jv+AnBft+9Q01UF07OydvgTTaTdCa+nMqabkUNl"
    ];
  };

  security.sudo.wheelNeedsPassword = false;

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

      genSomeCardanoKey = script: name: pkgs.writeShellScriptBin script ''
        if [ -f ${nodeId}-${name}.sk ]; then
          echo "Found a ${name} key for ${nodeId}; not generating another one."
          exit 0
        fi

        ${lib.getExe cardano-node.packages.${system}.cardano-cli} address key-gen \
          --verification-key-file ${nodeId}-${name}.vk \
          --signing-key-file ${nodeId}-${name}.sk
      '';

      genCardanoKey = genSomeCardanoKey "genCardanoKey" "node";
      genFundsKey = genSomeCardanoKey "genFundsKey" "funds";
    in
    [
      pkgs.git

      # So you can just do (if you just want fresh credentials):
      #
      #  > cd ~/cardano-data/credentials
      #  > genHydraKey
      #  > genCardanoKey
      #  > genFundsKey
      #
      genHydraKey
      genCardanoKey
      genFundsKey

      hydra.packages."${system}".hydra-tui # To interact with your node/peers

      # These aren't really needed, as the systemd services just pull in the
      # binaries directly, but might be useful for debugging, so we leave them
      # in the system path.
      hydra.packages."${system}".hydra-node # To run a hydra node
      cardano-node.packages."${system}".cardano-node # To talk to the cardano network
      mithril.packages."${system}".mithril-client-cli # Efficient syncing of the cardano node
      cardano-node.packages."${system}".cardano-cli # For any ad-hoc cardano actions we may like to run
    ];

  programs.bash.shellAliases = {
    # Run 'logs -f' to follow
    logs = "journalctl -u mithril-maybe-download -u cardano-node -u hydra-node -u necessary-files";

    # Always open hydra-tui with the right args:
    hydra-tui = "hydra-tui --testnet-magic ${networkMagic} --node-socket ${cardanoDataPath}/node.socket -k ${cardanoDataPath}/credentials/${nodeId}-funds.sk";
  };

  environment.variables = commonEnvVars;

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
                echo "Not re-creating configs because ${cardanoDataPath} exists."
                systemd-notify --ready
                exit 0
              fi

              mkdir -p ${cardanoDataPath}/credentials

              cd ${cardanoDataPath}

              # Get the node configs
              curl -L -O \
                https://github.com/IntersectMBO/cardano-node/releases/download/${nodeVersion}/cardano-node-${nodeVersion}-linux.tar.gz

              tar xf cardano-node-${nodeVersion}-linux.tar.gz \
                  ./share/${networkName} \
                  --strip-components=3

              # Get our hydra config (and peer config)
              git clone https://github.com/cardano-scaling/hydra-team-config.git

              # Jump to specific revision
              cd hydra-team-config && \
                git checkout dee7986b1377a0fa93d06cfc131ae7c26ca34299 && \
                cd ..

              systemd-notify --ready
            '';
          in
          "${lib.getExe necessaryFiles}";
      };
    };


    mithril-maybe-download =
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
        # We have to make a list here; this field doesn't support an attrset.
        Environment = lib.attrsets.mapAttrsToList (k: v: "${k}=${v}") commonEnvVars;
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
        # Wait 10 minutes before restarting
        RestartSec = 1 * 60;
        ExecStart =
          let
            peerArgs =
              let
                dir = "hydra-team-config/parties";
                f = name: lib.strings.concatStringsSep " "
                  [
                    "--peer $(cat ${dir}/${name}.peer)"
                    "--hydra-verification-key ${dir}/${name}.hydra.vk"
                    "--cardano-verification-key ${dir}/${name}.cardano.vk"
                  ];
              in
              pkgs.lib.strings.concatMapStringsSep " " f peers;
            spinupHydra = pkgs.writeShellScriptBin "spinupHydra" ''
              ${hydra.packages.${system}.hydra-node}/bin/hydra-node \
                --node-id ${nodeId} \
                --cardano-signing-key credentials/${nodeId}-node.sk \
                --hydra-signing-key credentials/${nodeId}-hydra.sk \
                --port ${hydraPort} \
                --api-host 0.0.0.0 \
                --host 0.0.0.0 \
                --testnet-magic ${networkMagic}  \
                --node-socket node.socket \
                --persistence-dir persistence \
                --ledger-protocol-parameters hydra-team-config/protocol-parameters.json \
                --hydra-scripts-tx-id $(cat hydra-team-config/hydra-scripts-tx-id) \
                ${peerArgs}
            '';
          in
          "${lib.getExe spinupHydra}";

        Restart = "on-failure";
      };
    };
  };
}
