# Example Hydra Node Infrastructure

This directory contains some [Terraform](https://www.hashicorp.com/products/terraform) and GCP based infrastructure code to:
* Setup a single [Hydra node](https://hydra.family/head-protocol/docs/installation),
* Connected to a [Cardano node](https://docs.cardano.org/getting-started/installing-the-cardano-node) running on `preview` testnet,
* Primed by [mithril](https://mithril.network) latest snapshot,
* Serving [hydraw](../../hydraw) application on port 80.

:warning: It's not a complete turnkey solution and requires some tweaking and parameterisation to be completely usable but we thought it would be good starting point for new Hydra users.

:warning: This should not be considered a production-grade setup esp. when it comes to security. Keys are being moved around freely in clear, there's no provision for TLS, application is exposed on port 80 without HTTPS, etc.

# Install

## GCP

terraform require access to GCP resources which is controlled by a _Service account_ configuration.

Assuming one has "admin" access to a GCP project, the following steps will create a service account, set the needed permissions and retrieve a key file which can then be used to configure the scripts:

Create the service account:

```
$ gcloud iam service-accounts create hydra-builder
```

Add needed permissions:

```
$ gcloud projects add-iam-policy-binding iog-hydra --member "serviceAccount:hydra@my-project.iam.gserviceaccount.com" --role "roles/compute.admin"
$ gcloud projects add-iam-policy-binding iog-hydra --member "serviceAccount:hydra@my-project.iam.gserviceaccount.com" --role "roles/iam.serviceAccountUser"
$ gcloud projects add-iam-policy-binding iog-hydra --member "serviceAccount:hydra@my-project.iam.gserviceaccount.com" --role "roles/compute.instanceAdmin.v1"
$ gcloud projects add-iam-policy-binding iog-hydra --member "serviceAccount:hydra@my-project.iam.gserviceaccount.com" --role "roles/storage.objectAdmin"
```

The service account must be able to create various `compute` instances, to modify the state which is stored inside a _Google Storage_ bucket, and to impersonate a service account user (unsure what this really means...).

Create service account's key file:

```
$ gcloud iam service-accounts keys create hydra.json --iam-account hydra@my-project.iam.gserviceaccount.com
```

## Selecting the base image

This can be done by editing the `image = iog-hydra-xxxx` parameter in [demo.tf](./demo.tf). The [configuration script](./scripts/configure-testnet.sh) assumes this image must be a reasonably recent Ubuntu/Debian Linux distribution, with [docker](https://docker.io) and [docker-compose](https://docs.docker.com/compose/) installed, and a configured user `curry` with `sudo` access.

## Deploying the VM

### Initialise Terraform

This should only be done once, when starting afresh.

```
$ terraform init -backend-config='bucket=my-bucket' -backend-config='prefix=my-prefix/in-bucket'
```

Update the `ssh_keys` file with public keys that will be allowed to log into the VM, prefixing each key with `curry` or `root` depending on whether one wants to provide normal user or super-user access to the VM.

Then create a deployment plan and apply it:

```
$ terraform plan -out vm.plan
```

### Configuring Hydra Node

The configuration process expects to find some files which are not provided by default and which are required for starting the Hydra node:
* A Hydra signing key file `keys/arnaud-hydra.sk` which will be used in the Head to sign snapshots.
  This can be generated using [hydra-node](https://hydra.family/head-protocol/docs/configuration#hydra-keyss),
* A cardano signing key file  `keys/arnaud.sk` which is required to identify the parties on-chain and sign transactions.
  This is a standard Cardano key so one can reuse an existing key or [generate a new one](https://hydra.family/head-protocol/docs/configuration#cardano-keys),
* 0 or more hydra verification keys and cardano verification keys for the other Head parties,
* The IP addresses and ports of _peer_ nodes,
* Configuration files [prometheus](https://prometheus.io/) which is run as part of the stack, for monitoring purpose,
* Configuration files for the off-chain ledger.

The key files should be put in a `keys/` directory. Then the [docker-compose.yaml](./docker-compose.yaml) should be edited to reflect the above parameters as [command-line arguments](https://hydra.family/head-protocol/docs/configuration) to the `hydra-node` container.

### Starting VM

```
$ terraform apply vm.plan
... <takes some more time>

Apply complete! Resources: 2 added, 0 changed, 0 destroyed.

Outputs:

instance_id = https://www.googleapis.com/compute/v1/projects/xxx
instance_ip = X.Y.Z.T
```

# Using the Hydra Node

## Login to the VM

One should be able to log into the VM as user `curry`.

To login to the VM:

```
$ scripts/login.sh curry@hydra-demo-1
```

## Using Hydraw

[Hydraw](../../hydraw/README.md) web interface is exposed on port 80. Pointing a web browser lets one interact with the UI to collaboratively draw pixels.

# Troubleshooting

Most issues boil down to authentication or authorisation problems.

> Cannot log in to the VM using `scripts/login.sh`

This script uses `GOOGLE_APPLICATION_CREDENTIALS` environment variable to activate the corresponding service account and use `gcloud compute ssh` to log in. Check authorizations of the service account.

> Cannot log in to the VM using plain `ssh`

* The set of authorized public keys is defined in the [ssh_keys](./ssh_keys) file: Check there is a private key corresponding to this public key. Changing the `ssh_keys` file and re-running `terraform apply` does not entail recreation of the VM so it's pretty fast
* If `ssh-agent` is running, check that a private key corresponding to an authorized public key is loaded with `ssh-add -l`

> Terraform fails to run `scripts/configure-testnet.sh` on the VM

Terraform relies on plain SSH to connect to the VM, so this can be caused by the same problems as the previous issue
