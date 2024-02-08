This explains how to install a cardano runner for the Hydra project.

This runner is used by our smoke-tests and allows us to keep on disk
an, as up to date as possible, cardano-node database.

# prepare pre-requisites

Install the following pre-requisites:
* git

For instance on Debian:
```bash
sudo apt install git
```

[Install Docker](https://docs.docker.com/engine/install/debian/#install-using-the-repository) 
and grant permissions to the user 'admin' to perform maintenance operations.
```bash
sudo usermod -aG docker $USER
```

Prepare the common directory for cardano database:
```bash
sudo mkdir -p /srv/var/cardano
sudo chown "$(whoami)" /srv/var/cardano
```

spin-up a passive cardano-node in preview
```bash
docker pull ghcr.io/input-output-hk/cardano-node:8.7.3
docker run -d --rm \
    --restart always \
    -v /srv/var/cardano/state-preview:/preview \
    -e CARDANO_SOCKET_PATH=/preview/node.socket \
    -e CARDANO_NODE_SOCKET_PATH=/preview/node.socket \
    ghcr.io/input-output-hk/cardano-node:8.7.3 \
    run \
    --config /preview/config.json \
    --topology /preview/topology.json \
    --database-path /preview/db
```

# Add the server as a github runner

In the project settings, go to Actions/Runners and click on [New self-hosted runner](https://github.com/input-output-hk/hydra/settings/actions/runners/new) and follow the procedure.

:warning: When asked, add the following label to this runner: `cardano`

# Customize github runner for nix

So that the jobs can find nix later, you should customize the runner settings by adding some
variables to the `.env` file:

```bash
cat <<EOF >>$HOME/actions-runner/.env
PATH=$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/bin
NIX_PROFILES="/nix/var/nix/profiles/default $HOME/.nix-profile"
NIX_SSL_CERT_FILE="/etc/ssl/certs/ca-certificates.crt"
EOF
```

# Install github runner as a systemd unit


So that github runner runs as a daemon on the machine, [install it](https://docs.github.com/en/actions/hosting-your-own-runners/configuring-the-self-hosted-runner-application-as-a-service):


```bash
sudo ./svc.sh install
```

You can now start the service:

```bash
sudo ./svc.sh start
```
