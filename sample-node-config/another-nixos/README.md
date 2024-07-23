# Another NixOS setup

A full NixOS-based image to run a Hydra Head with a group of peers.

It allows for:

- Deployment to GCP (by producing a GCE-compatible image; see <https://wiki.nixos.org/wiki/Install_NixOS_on_GCE> for steps to get the image onto GCP by uploading it to a bucket)
- Local-spin-up via qemu (for testing)
- A nixosConfiguration for re-building and redeploying via nixos-rebuild


### Redeploying

```shell
nixos-rebuild switch --target-host hydra@... --flake .#noon-hydra --use-remote-sudo
```


### Keys

Keys are expected to be generated/provided once it is spun up.

If you don't have keys already, you can generate them (one-off!) on the host
once it is up:

```shell
cd ~/cardano-data/credentials
genHydraKey
genCardanoKey
genFundsKey
```

The hydra-node service will restart until these are present.

If you do generate them on the host in this way, you will want to back them
up, just as you would with any secret.


### Building an image for GCE

```shell
nix build .#gce
```

This gives a .raw.tar.gz file that can be:

- Uploaded to a bucket,
- Then used to make a new image on GCP,
- Which can then be used as a boot image

See "Create a VM instance" here: <https://wiki.nixos.org/wiki/Install_NixOS_on_GCE>


### Trivia

qemu invocation tips:

```shell
nix build .#qemu
cp result/nixos.qcow2 .
chmod 755 nixos.qcow2
qemu-system-x86_64 -enable-kvm -m 8000 -drive file=nixos.qcow2,media=disk,if=virtio -nic user,model=virtio
```

Note that this results in very large nix store allocations; so if you do it
often don't forget to garbage-collect!
