*Disclaimer:* **`DO NOT USE IN PRODUCTION!`**

> This is a sample setup for demonstration only. Some sensitive data should be better protected in a production setup.


# Example Hydra Node Infrastructure

This directory contains some [Terraform](https://www.hashicorp.com/products/terraform) and AWS based infrastructure code to setup a single [Hydra node](https://hydra.family/head-protocol/docs/getting-started/installation) connected to a [Cardano node](https://docs.cardano.org/getting-started/installing-the-cardano-node) running on `preview` testnet. It's not a complete turnkey solution and requires some tweaking and parameterisation to be completely usable but we thought it would be good starting point for new Hydra users.

### Pre-requisites
1.  **AWS Account**: You need access to an AWS account with privileges to create EC2 instances and related resources.
2.  **AWS CLI & Terraform**: You must have the [AWS CLI](https://aws.amazon.com/cli/) and [Terraform](https://developer.hashicorp.com/terraform/downloads) installed and configured on your local machine.
3.  **AWS Credentials File**: Your local AWS credentials should be configured. For this example, we assume a profile in `~/.aws/credentials`:
    ```
    [personal]
    aws_access_key_id=???
    aws_secret_access_key=???
    region=eu-west-3
    ```
4.  **EC2 Key Pair**: You must create an EC2 Key Pair in the AWS region you intend to use. When you create it, AWS will provide a `.pem` file. **Download and save this file securely.** You will need its name for the configuration.
> Here and later we refer to your `AWS_PROFILE` name as **personal**.
> Whenever you see _personal_ in the rest of the file you need to replace it with your own aws profile name.
> Also you need to make sure to create a new key pair in the region eu-west-3 (TODO: abstract the region away?)
> TODO: it would be cool to use aws_ssm to connect to the EC2 so that we don't depend on extra ssh keys manipulations.

### Project Setup
Before running Terraform, you need to create a specific directory and file structure within the `sample-node-config/aws/` directory.

1.  **Create Directories**:
    - `credentials/`: This will store your Cardano-related keys.
    - `aws/`: This will store your EC2 `.pem` file.

2.  **Place Your Key**: Copy the `.pem` file you downloaded from AWS into the `aws/` directory. For example: `aws/my-key.pem`.

3.  **Generate Cardano Keys**: Create your Cardano keys and place them in the `credentials/` directory. You should have at least `cardano.addr`, `cardano-key.sk`, and `hydra-key.sk`.

The final structure should look like this:
```
.
+-- credentials/
|   +-- cardano.addr
|   +-- cardano-key.sk
|   +-- cardano-key.vk
|   +-- hydra-key.sk
|   +-- hydra-key.vk
+-- aws
|   +-- personal.pem
+-- terraform.tfvars
+-- (other *.tf files)
```

This should only be done once, when starting afresh.

TODO: refer to how to generate addresses and .sk, .vk files.

### Terraform Configuration

Instead of altering `variables.tf`, you should specify your custom values in a `terraform.tfvars` file at the root of the `sample-node-config/aws/` directory.

At the root of the `aws` project, execute:
```sh
$ ./setup/setup.sh
```
[Note] all scripts inside the `/setup` folder are expected to be executed at the root of the `aws` project.

This will create a file called `terraform.tfvars`, used to complete the `variables.tf` definitions. For this example it would be defined as:
```toml
# The AWS profile name from your ~/.aws/credentials file.
profile    = "iog"
# The name of the EC2 Key Pair you created in the AWS console.
# This must match the name of the key pair, not the filename.
key_name   = "personal"
```

## Initialise Terraform

This should only be done once, when starting afresh.
At the root of `aws` project execute:

```sh
terraform init
```

### Alter terraform variables

Instead of altering `variables.tf`, you should specify your custom values in a `terraform.tfvars` file at the root of the `sample-node-config/aws/` directory.
Alter the file `variables.tf` found in `sample-node-config/aws/` to reflect your ami, region and instance_type. Defaults are already provided.

Example 
```toml
# The AWS profile from your `~/.aws/credentials` file.
profile       = "default"
# The name of the EC2 Key Pair you created in the AWS Console for your chosen region.
key_name      = "hydra-node-key"
# The hydra network environment to use. Can be "preview", "preprod", or "mainnet".
env           = "preview"
# The AWS region where you created your key pair and want to deploy resources.
region        = "eu-central-1"
# The Amazon Machine Image (AMI) ID for a recent Ubuntu release in your chosen region.
# You can find this in the AWS EC2 Console when launching a new instance.
ami           = "ami-012345fxxxxxxxx"
# The type of EC2 instance to launch.
instance_type = "r5a.xlarge"
```

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

```shell
source ~/.bash_env
```

## Create marker utxo: get fuel from testnet
Now that you are logged in to your VM, first thing we need is to spin up your `cardano-node` and create the marker utxo.


For that we will need to executea docker command. First, go to the `docker folder:
```shell
cd docker
```

and then:
```shell
$ docker-compose up -d cardano-node
$ fuel
```

## Configuring Hydra Node

We need to make sure some files, which are not provided by default and which are required for starting the `hydra-node`, are in place at the home folder of your VM:
* A Hydra signing key file `hydra-key.sk` which will be used in the Head to sign snapshots.
  This can be generated using [hydra-node](https://hydra.family/head-protocol/docs/configuration#hydra-keys),
* A cardano signing key file  `cardano-key.sk` which is required to identify the parties on-chain and sign transactions.
  This is a standard Cardano key so one can reuse an existing key or [generate a new one](https://hydra.family/head-protocol/docs/configuration#cardano-keys),
* 0 or more hydra verification keys and cardano verification keys for the other Head parties,
* The IP addresses and ports of _peer_ nodes,
* Configuration files for [promtail](https://grafana.com/docs/loki/latest/clients/promtail/) and [prometheus](https://prometheus.io/) which are run as part of the stack,
* Configuration files for the off-chain ledger.

Then the [docker-compose.yaml](./docker/docker-compose.yaml) should be edited to reflect the above parameters as [command-line arguments](https://hydra.family/head-protocol/docs/configuration) to the `hydra-node` container.

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

```sh
up
```

> execute `down` to take it down

## Opening the Head
Finally, execute the hydra-tui and open the head:

```sh
tui
```

> If you take down your hydra-node instance once the head is open. you will loose access to your funds committed to the head.
> To get them back, currently you need to start the head from a point time in the past.
> For that you must run hydra-node the using parameter `--start-chain-from`.
> i.e.: `-start-chain-from 2730515.c7a3629911ef004c873ef07313842df5d1331f61e0eb632432ac8c0636dfd391`

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

```sh
sync
```

But others may come from the containers itself.
For that, a CloudWatch Agent is configured to publish the docker logs to AWS CloudWatch service, under `instance_logs` log group.
See the [agent config](./cw_agent_config.json) for more details.
