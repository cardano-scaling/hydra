# Hydraw running on hydra running on cardano

{ pkgs, ... }:

let
  cardano-node = import
    (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "1.35.4";
      sha256 = "1j01m2cp2vdcl26zx9xmipr551v3b2rz9kfn9ik8byfwj1z7652r";
    })
    { };
in
{
  # Add iohk substituters
  nix.settings.substituters = [
    "https://cache.nixos.org"
    "https://cache.iog.io"
  ];
  nix.settings.trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];

  # The 1.35.3-new cardano-node image does not contain a cli, so let's add it
  # using nix instead.
  environment.systemPackages = [ cardano-node.cardano-cli ];

  # Using entrypoint from: https://github.com/input-output-hk/cardano-world/blob/master/nix/cardano/entrypoints.nix
  virtualisation.oci-containers.containers = {
    cardano-node = {
      image = "inputoutput/cardano-node:1.35.3-new";
      volumes = [
        "/data/cardano-node:/data"
      ];
      environment = {
        DATA_DIR = "/data";
        ENVIRONMENT = "preview";
        SOCKET_PATH = "/data/node.socket";
      };
    };


    # Our hydra-node instance
    hydra-node =
      # NOTE: These are for the "preview" network
      let
        hydraScriptsTxId = "bde2ca1f404200e78202ec37979174df9941e96fd35c05b3680d79465853a246";
        networkMagic = "2";
      in
      {
        image = "ghcr.io/input-output-hk/hydra-node:0.8.1";
        volumes = [
          "/data/cardano-node:/cardano-node:ro"
          "/data/hydra-node:/data:ro"
        ];
        ports = [
          "4001:4001"
          "5001:5001"
        ];
        cmd = builtins.concatLists [
          [ "--node-id" "314" ]
          [ "--api-host" "0.0.0.0" ]
          [ "--host" "0.0.0.0" ]
          [ "--monitoring-port" "6001" ]
          [ "--hydra-scripts-tx-id" hydraScriptsTxId ]
          [ "--hydra-signing-key" "/data/credentials/sebastian.hydra.sk" ]
          [ "--cardano-signing-key" "/data/credentials/sebastian.cardano.sk" ]
          [ "--ledger-protocol-parameters" "/data/protocol-parameters.json" ]
          [ "--testnet-magic" networkMagic ]
          [ "--node-socket" "/cardano-node/node.socket" ]
        ];
      };

    # The hydraw application / bridge
    hydraw = {
      image = "ghcr.io/cardano-scaling/hydraw:latest";
      volumes = [
        "/data/hydra-node/credentials:/credentials:ro"
      ];
      entrypoint = "hydraw";
      environment = {
        HYDRAW_CARDANO_SIGNING_KEY = "/credentials/sebastian.cardano.sk";
        HYDRA_API_HOST = "localhost:4001";
      };
      extraOptions = [ "--network=host" ];
    };
  };

  # Configure the reverse proxy to point at it
  services.nginx.virtualHosts."hydraw.fk.ncoding.at" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:1337";
      proxyWebsockets = true;
      extraConfig = ''
        proxy_buffering off;
        client_max_body_size 500M;
      '';
    };
  };
}
