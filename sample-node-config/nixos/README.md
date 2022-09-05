This directory does contain an example configuration to run a `cardano-node`,
`hydra-node` and the example `hydraw` application on a NixOS host.

It is not using fancy NixOps or proper NixOS services, but the basic declarative
`virtualisation.oci-containers` NixOS options to make it very similar to the
`docker-compose` way of orchestrating the containers.

## Add & start containers

To use this, copy or symlink the `hydraw.nix` module and import it into your `NixOS`' `configuration.nix`:

```nix
{ config, pkgs, lib, ... }:

{
  imports = [
    ./hydraw.nix
  ];
  
  # ... the rest of your configuration.nix
}
```

Then, after the next `nixos-rebuild switch`, the relevant docker containers should be started by systemd and you can see their logs in either:

``` sh
journalctl -u docker-cardano-node.service
docker logs cardano-node
```

## Copying credentials

As not everything is automated yet, we will need to copy the configured
credentials of `hydraw.nix` to the target host and create relevant directories.
For example using `ssh` and `scp` with hostname `example`:

``` sh
ssh example "mkdir -p /data/cardano-node /data/hydra-node/credentials"
scp sebastian.cardano.sk sebastian.hydra.sk example:/data/hydra-node/credentials/
```

Also copy any additional Hydra verification keys into the
`/data/hydra-node/credentials` and extend the command line of the `hydra-node`
container accordingly.

Lastly, the `hydra-node` requires some `protocol-parameters.json` to configure the ledger in the `/data/hydra-node` directory. Use the ones from the e2e tests `hydra-cluster/config/protocol-parameters.json` or use the L1 ones via the `cardano-cli` on the target host:

``` sh
ssh example "CARDANO_NODE_SOCKET_PATH=/data/cardano-node/node.socket cardano-cli query protocol-parameters --testnet-magic 2 > /data/hydra-node/protocol-parameters.json"
```

## Attach the tui

For this, we do not need a service definition, but can simply use `docker` via SSH:

``` sh
ssh -t example docker run --rm -it \
  -v /data:/data \
  --network host \
  ghcr.io/input-output-hk/hydra-tui:0.7.0 \
  --connect 0.0.0.0:4001 \
  --node-socket /data/cardano-node/node.socket \
  --network-id 2 \
  --cardano-signing-key /data/hydra-node/credentials/sebastian.cardano.sk
```
