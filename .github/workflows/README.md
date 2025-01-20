This explains how to install a cardano runner for the Hydra project.

This runner is used by our smoke-tests and allows us to keep on disk an, as up to date as possible, cardano-node database.

# Instance requirements

To host cardano-node instances for `preview`, `preprod` and `mainnet` we need *at least*:

- 150GB of disk space
- 18GB of RAM 

In AWS an `r5.xlarge` instance type would fit.

# Instance access

Use a shared key to create the instance, connect to it and ensure core
contributors have access to it using their SSH keys from github. For example:

``` shell
for GHUSER in ch1bo ffakenz v0d1ch locallycompact noonio; do
  echo "# ${GHUSER}" >> ~/.ssh/authorized_keys
  curl https://github.com/${GHUSER}.keys >> ~/.ssh/authorized_keys
done
```

# Prepare pre-requisites

Install the following pre-requisites:
* `git`
* `docker`
* `docker compose`

For instance on Ubuntu:
```bash
sudo apt install git docker.io docker-compose-v2
```

Ensure we can run docker commands:

``` bash
sudo usermod -aG docker $(whoami)
```

Prepare the common directory for cardano database:
```bash
sudo mkdir -p /data/cardano
sudo chown "$(whoami)" /data/cardano
```

# Add the server as a github runner

In the project settings, go to Actions/Runners and click on [New self-hosted runner](https://github.com/cardano-scaling/hydra/settings/actions/runners/new) and follow the procedure.

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

Nix should be installed by our actions. However we saw that the github token installed into `/etc/nix/nix.conf` might not be kept up-to-date. Removing it seems to be fine

``` shell
sudo sed '/access-tokens/d' -i /etc/nix/nix.conf
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
