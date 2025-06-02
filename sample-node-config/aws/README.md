*Disclaimer:* **`DO NOT USE IN PRODUCTION!`**

> This is a sample setup for demonstration only. Some sensitive data should be better protected in a production setup.


# Example Hydra Node Infrastructure

This directory contains some [Terraform](https://www.hashicorp.com/products/terraform) and AWS based infrastructure code to setup a single [Hydra node](https://hydra.family/head-protocol/docs/getting-started/installation) connected to a [Cardano node](https://docs.cardano.org/getting-started/installing-the-cardano-node) running on `preview` testnet. It's not a complete turnkey solution and requires some tweaking and parameterisation to be completely usable but we thought it would be good starting point for new Hydra users.

### Pre-requisites
- you have access to an aws account with root privileges.
- you have configured your local aws credentials.
for this example in `~/.aws/credentials` we have:
    ```
    [personal]
    aws_access_key_id=???
    aws_secret_access_key=???
    region=eu-west-3
    ```
> **personal** will be your AWS_PROFILE
> so whenever you see _personal_ in the rest of the file you need to replace it with your own aws profile name.
> Also you need to make sure to create a new key pair in the region eu-west-3 (TODO: abstract the region away?)
> TODO: it would be cool to use aws_ssm to connect to the EC2 so that we don't depend on extra ssh keys manipulations.

- you have terraform and aws installed on your system.
- you have built the required 2 folder structures under aws folder:
    + credentials.
    + aws.

## Project structure to build
```
.
+-- credentials
|   +-- arnaud.cardano.vk
|   +-- arnaud.hydra.vk
|   +-- cardano-key.sk
|   +-- cardano-key.vk
|   +-- cardano.addr
|   +-- hydra-key.sk
|   +-- hydra-key.vk
+-- aws
|   +-- personal.pem
+-- terraform.tfvars
```

This should only be done once, when starting afresh.

These are folders you must create yourself:
- ***credentials*** to store all blockchain related credential files.
- ***aws*** to store your personal aws credentials.

At the root of the `aws` project, execute:
```sh
$ ./setup/setup.sh
```
[Note] all scripts inside the `/setup` folder are expected to be executed at the root of the `aws` project.

This will create a file called `terraform.tfvars`, used to complete the `variables.tf` definitions. For this example it would be defined as:
```
profile    = "iog"
key_name   = "personal"
```

## Initialise Terraform
This should only be done once, when starting afresh.
At the root of `aws` project execute:
```sh
$ terraform init
```

### Alter terraform variables
Alter the file `variables.tf` found in `sample-node-config/aws/` to reflect your ami, region and instance_type. Defaults are already provided.

### Deploying the VM
Then create a deployment plan and apply it:
```sh
$ terraform plan -out vm.plan
```

#### Starting VM

```sh
$ terraform apply vm.plan
... <takes some time - 3m approx>

Apply complete! Resources: 19 added, 0 changed, 0 destroyed.

Outputs:

instance_dns = "ec2-13-38-249-96.eu-west-3.compute.amazonaws.com"
instance_ip = "13.37.88.84"
```

> execute `terraform destroy` to take it down

# Using the Hydra Node
Before you open a head you must have funds on the preview network and then
prepare the funds to be marked as fuel.

To get some funds from the preview faucet to your address, you can either:
    + claim them from site:
        https://faucet.preview.world.dev.cardano.org/basic-faucet
    + or request them via http:
        ```sh
        $ curl -X POST -s "https://faucet.preview.world.dev.cardano.org/send-money/$(cat credentials/cardano.addr)?api_key="
        ```

        > to request via http you must first obtain your api_key.

Note: From now on we are assuming that the following commands will be executed inside of `sample-node-config/aws`

``` sh
$ cd sample-node-config/aws
```

## Login to the VM

One should be able to log into the VM as user `ubuntu`.

Note: Please make sure your `aws/personal.pem` file has correct permissions (`chmod 400 aws/personal.pem`)

To login to the VM:

```
$ scripts/login.sh
```

> useful tmux commands:
- ctrl B + D: detach session
- tmux a: attach the detached-session

## Create marker utxo: get fuel from testnet
Now that you are logged in to your VM, first thing we need is to spin up your `cardano-node` and create the marker utxo.
For that we will need to execute:
```
$ docker-compose up -d cardano-node
$ fuel
```

## Configuring Hydra Node
We need to make sure some files, which are not provided by default and which are required for starting the `hydra-node`, are in place at the home folder of your VM:
* A Hydra signing key file `hydra-key.sk` which will be used in the Head to sign snapshots.
  This can be generated using [hydra-node](https://hydra.family/head-protocol/docs/getting-started/quickstart#hydra-keys),
* A cardano signing key file  `cardano-key.sk` which is required to identify the parties on-chain and sign transactions.
  This is a standard Cardano key so one can reuse an existing key or [generate a new one](https://hydra.family/head-protocol/docs/getting-started/quickstart#cardano-keys),
* 0 or more hydra verification keys and cardano verification keys for the other Head parties,
* The IP addresses and ports of _peer_ nodes,
* Configuration files for [promtail](https://grafana.com/docs/loki/latest/clients/promtail/) and [prometheus](https://prometheus.io/) which are run as part of the stack,
* Configuration files for the off-chain ledger.

Then the [docker-compose.yaml](./docker/docker-compose.yaml) should be edited to reflect the above parameters as [command-line arguments](https://hydra.family/head-protocol/docs/getting-started/quickstart) to the `hydra-node` container.

i.e.:
```
"--peer", "35.233.17.169:5001"
"--hydra-verification-key", "/data/arnaud.hydra.vk"
"--cardano-verification-key", "/data/arnaud.cardano.vk"
```

The [promtail-config.yml](./docker/promtail-config.yml) should be edited to point to the correct URL where logs should be shipped or the promtail container altogether removed.

For `cardano-node` and `hydra-node` services, make sure the `logging` options for `awslogs` are properly aligned with what is defined in your `cloudwatch.tf` and `variables.tf` files.

## Running the hydraw instance
Next, to run hydraw, execute:
```
$ up
```

> execute `down` to take it down

## Opening the Head
Finally, execute the hydra-tui and open the head:
```
$ tui
```

> If you take down your hydra-node instance once the head is open. you will loose access to your funds committed to the head.
To get them back, currently you need to start the head from a point time in the past.
For that you must run hydra-node the using parameter `--start-chain-from`.
i.e.: --start-chain-from 2730515.c7a3629911ef004c873ef07313842df5d1331f61e0eb632432ac8c0636dfd391

## Using Hydraw

[Hydraw](../../hydraw/README.md) web interface is exposed on port 80. Pointing a web browser lets one interact with the UI to collaboratively draw pixels.

# Troubleshooting

Most issues boil down to authentication or authorisation problems.

> Cannot log in to the VM using `scripts/login.sh`

This script uses `ssh` to log in. Check authorizations of the service account.

> Terraform fails to run `scripts/configure-testnet.sh` on the VM

Terraform relies on plain SSH to connect to the VM, so this can be caused by the same problems as the previous issue

> Peers cannot see you connected
Perhaps your node is out of sync with the L1.
Check sync progress by running a `cardano-cli` query tip on your cardano node:
```
$ sync
```

But others may come from the containers itself.
For that, a CloudWatch Agent is configured to publish the docker logs to AWS CloudWatch service, under `instance_logs` log group.
See the [agent config](./cw_agent_config.json) for more details.
