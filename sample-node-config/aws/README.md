# Example Hydra Node Infrastructure

This directory contains some [Terraform](https://www.hashicorp.com/products/terraform) and AWS based infrastructure code to setup a single [Hydra node](https://hydra.family/head-protocol/docs/getting-started/installation) connected to a [Cardano node](https://docs.cardano.org/getting-started/installing-the-cardano-node) running on `preview` testnet. It's not a complete turnkey solution and requires some tweaking and parameterisation to be completely usable but we thought it would be good starting point for new Hydra users.

## Project structure tu build
```
.
+-- credentials
|   +-- arnaud-cardano.vk
|   +-- arnaud-hydra.vk
|   +-- cardano.addr
|   +-- cardano-key.sk
|   +-- cardano-key.vk
|   +-- hydra-key.sk
|   +-- hydra-key.vk
+-- docker
|   +-- docker-compose.yaml
|   +-- prometheus.yml
|   +-- promtail-config.yml
+-- env
|   +-- personal.pem
+-- scripts
|   +-- configure-instance.sh
|   +-- configure-testnet.sh
|   +-- fuel-testnet.sh
|   +-- login.sh
|   +-- scp.sh
+-- network.tf
+-- testnet.tf
```

## Configuring Hydra Node
The [configuration script](./scripts/configure-testnet.sh) assumes this image must be a reasonably recent Ubuntu/Debian Linux distribution, with [docker](https://docker.io) and [docker-compose](https://docs.docker.com/compose/) installed, and a configured user `ubuntu` with `sudo` access.

The configuration process expects to find some files which are not provided by default and which are required for starting the Hydra node:
* A Hydra signing key file `hydra-key.sk` which will be used in the Head to sign snapshots.
  This can be generated using [hydra-tools](https://hydra.family/head-protocol/docs/getting-started/quickstart#hydra-keys),
* A cardano signing key file  `cardano-key.sk` which is required to identify the parties on-chain and sign transactions.
  This is a standard Cardano key so one can reuse an existing key or [generate a new one](https://hydra.family/head-protocol/docs/getting-started/quickstart#cardano-keys),
* 0 or more hydra verification keys and cardano verification keys for the other Head parties,
* The IP addresses and ports of _peer_ nodes,
* Configuration files for [promtail](https://grafana.com/docs/loki/latest/clients/promtail/) and [prometheus](https://prometheus.io/) which are run as part of the stack,
* Configuration files for the off-chain ledger.

The key files should be put in the current directory and their name referenced in the [testnet.tf](./testnet.tf) file. Then the [docker-compose.yaml](./docker/docker-compose.yaml) should be edited to reflect the above parameters as [command-line arguments](https://hydra.family/head-protocol/docs/getting-started/quickstart) to the `hydra-node` container.

The [promtail-config.yml](./docker/promtail-config.yml) should be edited to point to the correct URL where logs should be shipped or the promtail container altogether removed.

### Pre-requisites
- you have access to an aws account with root priviledges.
- you have configured your local aws credentials. 
for this example in `~/.aws/credentials` we have:
    ```
    [personal]
    aws_access_key_id=???
    aws_secret_access_key=???
    region=eu-west-3
    ```
- you have terraform installed on your system.
- you have build the required 2 folder structures under aws:
    + credentials.
    + env.
- you have claimed some funds from the preview faucet to your address.
you can get them either by:
    + claim funds from site:
        https://faucet.preview.world.dev.cardano.org/basic-faucet
    + request them via http: 
        ```sh
        $ curl -X POST -s "https://faucet.preview.world.dev.cardano.org/send-money/$(cat credentials/cardano.addr)?api_key="
        ```
    
        > to request via http you must first obtain your api_key.

### Building ***credentials***
This should only be done once, when starting afresh.
This is a folder you must create yourself to store all blockchain related credential files.

1. ask to your party members for their verification keys (for this template, the party member is ***arnaud***)

2. generate your cardano signing and verification keys.
at the root of `hydra-poc` project, execute:
    ```sh
    $ cardano-cli address key-gen --signing-key-file cardano-key.sk --verification-key-file cardano-key.vk
    ```
    > `cardano-key.sk` and `cardano-key.vk` will be generated at the root of the `hydra-poc` project.

3. generate your hydra signing and verification keys.
at the root of `hydra-poc` project, execute:
    ```sh
    $ cabal run hydra-tools gen-hydra-key
    ```
    > `hydra-key.sk` and `hydra-key.vk` will be generated at the root of the `hydra-poc` project.

4. generate your cardano address
at the root of `hydra-poc` project, execute:
    ```sh
    $ cardano-cli address build --payment-verification-key-file ./sample-node-config/aws/credentials/cardano-key.vk --testnet-magic 1 > cardano.addr
    ```
    > `cardano.addr` will be generated at the root of the `hydra-poc` project.

5. move under `credetials` folder:
    - the party members verification keys: `arnaud-cardano.vk` and `arnaud-hydra.vk`.
    - your personal hydra keys: `hydra-key.sk` and `hydra-key.vk`.
    - your personal cardano keys: `cardano-key.sk` and `cardano-key.vk`.
    - your personal cardano address: `cardano.addr`.

### Building ***env***
This should only be done once, when starting afresh.
This is a folder you must create yourself to store your personal aws credentials.

go to ***EC2 Dashboard*** > ***Network & Security*** > ***Key Pairs*** and create one (for this template, the key pair is ***personal***). 
Then move it under `env` folder.

> Default values are good enough (type: RSA & OpenSSH format: *.pem).

> Creating a new pair will automatically generate a file and download it (one time action).

## Initialise Terraform
This should only be done once, when starting afresh.
At the root of `aws` project execute:
```sh
$ terraform init
```

### Deploying the VM
Then create a deployment plan and apply it:
```sh
$ terraform plan -out vm.plan
```

#### Starting VM

```sh
$ terraform apply vm.plan
... <takes some time - 3m aprox>

Apply complete! Resources: 7 added, 0 changed, 0 destroyed.

Outputs:

instance_ip = "ec2-13-38-62-128.eu-west-3.compute.amazonaws.com"
```

> execute `terraform destroy` to take it down


# Using the Hydra Node

## Login to the VM

One should be able to log into the VM as user `ubuntu`.

To login to the VM:

```
$ scripts/login.sh
```

> useful tmux commands:
- ctrl B + D: detach session
- tmux a: attach the detached-session

## Prepare the funds to be marked as fuel - on demand
Once you login to your VM, execute:
```sh
$ # create marker utxo
$ sudo chmod +x ./fuel-testnet.sh
$ exec ./fuel-testnet.sh devnet cardano-key.sk 10000000
```

## Opening the Head
Once you login to your VM, execute the hydra-tui and open the head:
```sh
$ docker-compose --profile tui run hydra-tui
```

> If you take down your hydra-node instance while the head is open. you will loose access to your funds commited to the head. 
To get them back, currently you need to start the head from a point time in the past.
For that you must run hydra-node the using parameter `--start-chain-from`.
i.e.: --start-chain-from 2730515.c7a3629911ef004c873ef07313842df5d1331f61e0eb632432ac8c0636dfd391

## Using Hydraw

[Hydraw](../../hydraw/README.md) web interface is exposed on port 80. Pointing a web browser lets one interact with the UI to collaboratively draw pixels.

# Troubleshooting

Most issues boil down to authentication or authorisation problems.

> Cannot log in to the VM using `scripts/login.sh`

This script uses `AWS_PROFILE` environment variable to activate the corresponding service account and use `ssh` to log in. Check authorizations of the service account.

> Terraform fails to run `scripts/configure-testnet.sh` on the VM

Terraform relies on plain SSH to connect to the VM, so this can be caused by the same problems as the previous issue
