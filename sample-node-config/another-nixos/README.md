# Another NixOS setup

A Hydra signing key needs to be generated on the host, once it is spun up.
It is expected to be created in `~/cardano-data/credentials` like so:

```shell
cd ~/cardano-data
hydra-node gen-hydra-key --output-file credentials/noon-hydra
```

where `noon` should be the value you have set as the `nodeId` in
`configuration.nix`

Once created, the service will restart and then will be up and running!

### Trivia

qemu invocation tips:

```shell
nix build .#qemu
cp result/nixos.qcow2 .
chmod 755 nixos.qcow2
qemu-system-x86_64 -enable-kvm -m 8000 -drive file=nixos.qcow2,media=disk,if=virtio -nic user,model=virtio
```

### Todo

- [ ] Get setup for remote nixos-rebuilds as well. Presently this creates an
      image, but assumes that it won't every be changed. It might be nice to
      also add a way to do a nixos-rebuild so that we can deploy changes to a
      deployed instance.
