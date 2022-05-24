Example configuration to connect to the Vasil Testnet

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
./fuel-testnet.sh credentials/sebastian.cardano.sk 500000000
cardano-cli transaction submit --cardano-mode --epoch-slots 21600 --testnet-magic 9 --tx-file /tmp/tmp.S9N6vVkw2j.signed
```

To start the `hydra-tui`:

``` sh
./hydra-tui.sh
```

Now we should be able to `[i]nit`, `[c]ommit` & `[c]lose` a hydra head using the interactive terminal client.


## First run on vasil testnet

``` sh
git rev: 4112202d009a1042370a7761e58393c59638fbab
start point: 0853d12dc3ac56ebfa1edae5207aafe4849ea6e6534275c7345047a6cf72814c at slot 400208
```

```
init tx: 65b8d0a9a325e8e54c5dea0f9b4a26dacb429959290f6d2914fb824f2db8e8a1
v_head address: addr_test1wqnt9a4jxyzxy3nfd79fzftn8kf3n3jfe6p8ethf625ezncxlpxyu
```

The `commit` or `abort` transactions fail with a `NonOutputSupplimentaryDatums` ledger error.

## Second run with non-inline datums

``` sh
git rev: 938c4c2da68eb908b1be7d2d12e6fd76de0d167b
start point: 7870838f05071a30b364832490cfbf9f882382a7d5524e821debaad099c4ed99 at slot 440772
```

```
init tx: a1b274bfd4f140f41765d553d6a1bdaa8e29da436f29c7c4caae2b9abc233858
v_head address: addr_test1wredcw3m2qyz68anfcjshcdljwaksj492254dw8pdce6n2seyjhg5

commit tx: 5e90b4233896814d93c3b9c712a54eee63a003de63aa6ada7d855a07c07d83fb
collect tx: baa721e3e3142cca17a3bbda4f2160ef0b5210eb781ae1b9556ebe87eae66409
```

Head is open!
