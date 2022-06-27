Configuration to connect to testnet

``` shell
export MAGIC=1097911063
```

Includes scripts to launch a recent enough `cardano-node`, `hydra-node` and
`hydra-tui`. Make sure they are in scope, e.g. by `cabal build
hydra-node:exe:hydra-node`.

First start a cardano-node and see whether it synchronizes:

```sh
./cardano-node.sh
```

Then, you need some `credentials/` as indicated by the scripts. See [the user
manual](https://hydra.family/head-protocol/docs/getting-started/quickstart#and-where-to-find-them)
on how to get these. Update the references to your credentials accordingly and
launch the `hydra-node`:

``` sh
./hydra-node.sh
```

Now, we need funds on the testnet and have them sent to an address owned by our `cardano-signing-key` in order to open a Hydra Head. With these available we can mark them as fuel and fork of some ADA to be committable into a hydra head. In this example we prepare some 500ADA (minus fees):

``` sh
export CARDANO_NODE_SOCKET_PATH=ipc/node.socket
./fuel-testnet.sh credentials/sebastian.cardano.sk 500000000
cardano-cli transaction submit --cardano-mode --epoch-slots 21600 --testnet-magic ${MAGIC} --tx-file /tmp/tmp.S9N6vVkw2j.signed
```

To start the `hydra-tui`:

``` sh
./hydra-tui.sh
```

Now we should be able to `[i]nit`, `[c]ommit` & `[c]lose` a hydra head using the interactive terminal client.
