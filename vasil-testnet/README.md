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

## Third run with rebased babbage-preview

``` sh
git rev: 283c5301e58a0e61dd0daa4691336e69c143c063
start point: 27eb7e85379a0021f3bd80081b3af75beacc256deb016c777d6b1b4f2ef92d62 at slot 1076650
```

```
init tx: 7c0c2427c0c31d426aa867fe0218139a8a5ebff74600fd439a574eb04ccea651
v_head address: addr_test1wqnggtxk9v8pn5uj658ekk95cqcxlu0a3tdmzelxvzzf0aqvr599t

commit tx: 37c88fa7c50403fa02aa8978d7c512fab96170024345ba8a37818ab6a833b33a
collect tx: 1eaa95e91ce450c22a184d6746c29a82f1d581e00f3e4d0d67595d24ba147dd1
```

Head is open!

Made two transactions paying myself resulting in off-chain utxo:

```json
{
  "542e01c8374232bc6543490a70067d10ff5be2114fb3289c8f532c2967a68630#0": {
    "address": "addr_test1vp2l229athdj05l20ggnqz24p4ltlj55e7n4xplt2mxw8tqt3ruuw",
    "datum": null,
    "datumhash": null,
    "inlineDatum": null,
    "referenceScript": null,
    "value": {
      "lovelace": 20000000
    }
  },
  "542e01c8374232bc6543490a70067d10ff5be2114fb3289c8f532c2967a68630#1": {
    "address": "addr_test1vp2l229athdj05l20ggnqz24p4ltlj55e7n4xplt2mxw8tqt3ruuw",
    "datum": null,
    "datumhash": null,
    "inlineDatum": null,
    "referenceScript": null,
    "value": {
      "lovelace": 69832211
    }
  },
  "d7cd0cd6fee49548df03c4a98dce7fae4ff45f95718f22f691ea0366c70eec19#0": {
    "address": "addr_test1vp2l229athdj05l20ggnqz24p4ltlj55e7n4xplt2mxw8tqt3ruuw",
    "datum": null,
    "datumhash": null,
    "inlineDatum": null,
    "referenceScript": null,
    "value": {
      "lovelace": 10000000
    }
  }
}
```

Closed the head with tx `6410896bf60b0b17045cc1a0c8aaada243009bd28f5c181caf54e5a584848b05`, but it directly rolled back! The tx was still included and observed -> Head is closed.

After contestation fanout using tx `458414a0ef14b95126594ea5a3d04a900d2afa91103d531c985abb03002551cc` -> Head finalized and all funds distributed.
