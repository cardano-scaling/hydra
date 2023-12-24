Configurations to run hydra on the public testnets

Includes scripts to launch a recent enough `cardano-node`, `hydra-node` and
`hydra-tui`. Make sure they are in scope, e.g. by `cabal build hydra-node:exe:hydra-node` or using the `nix develop .#demo`.

Let's assume you want to run a simple scenario on the `preview` testnet with a single actor. If you have multiple parties, the fueling needs to be done for each party's `hydra-node` and configuration needs to be adapted accordingly below.

First start a cardano-node and see whether it synchronizes:

```sh
./cardano-node.sh preview
```

A minimal scenario would be a single actor running a hydra-node on the testnet.

Now, we need funds on the testnet and have them sent to an address owned by our `--cardano-signing-key` in order to open a Hydra Head.

For `preview` we can use this faucet request:

```sh
curl -X POST -s "https://faucet.preview.world.dev.cardano.org/send-money/$(cat credentials/hydra-fuel.address)?api_key=oochuyai3ku4Fei4ahth9ooch9ohth7d"
```

Now we need to locate the Hydra scripts for the given testnet. For tagged &
released versions we will publish them on the respective [release
page](https://github.com/input-output-hk/hydra/releases).

You can also publish them yourselves by paying some some 50ADA and using this
command and keep:

```sh
hydra-node publish-scripts \
    --testnet-magic $(cat preview/db/protocolMagicId) \
    --node-socket "preview/node.socket" \
    --cardano-signing-key credentials/sebastian.cardano.sk
```

For the sake of this example let's assume one:

```sh
export HYDRA_SCRIPTS_TX_ID="fd5de60b31651ed1747cf911ce6e2df43d5181c139d606e37eb58bbd9ecbbf5b"
```

To spin up the `hydra-node` you need the protocol parameters that the ledger in
our Hydra head will use. You can use the same parameters as on the Cardano
layer one, but usually we want to have zeroed fees. For this we will use
cardano-cli. This will fetch the parameters and sets fees + prices to zero:

```
cardano-cli query protocol-parameters \
  | jq '.txFeeFixed = 0 |.txFeePerByte = 0 | .executionUnitPrices.priceMemory = 0 | .executionUnitPrices.priceSteps = 0' \
  > protocol-parameters.json
```

Then, you need some hydra credentials as indicated by the scripts. See [the user
manual](https://hydra.family/head-protocol/docs/getting-started/quickstart#and-where-to-find-them)
on how to get these. Update the references to your credentials accordingly and
launch the `hydra-node`:

```sh
hydra-node \
  --node-id 314 \
  --node-socket "preview/node.socket" \
  --testnet-magic $(cat preview/db/protocolMagicId) \
  --hydra-scripts-tx-id ${HYDRA_SCRIPTS_TX_ID} \
  --ledger-protocol-parameters "protocol-parameters.json" \
  --hydra-signing-key "credentials/sebastian.hydra.sk" \
  --cardano-signing-key "credentials/sebastian.cardano.sk"
```

To start the `hydra-tui`:

```sh
hydra-tui \
  --connect "0.0.0.0:4001" \
  --cardano-signing-key "credentials/sebastian.cardano.sk" \
  --testnet-magic $(cat preview/db/protocolMagicId) \
  --node-socket "preview/node.socket"
```

Now we should be able to `[i]nit`, `[c]ommit` & `[c]lose` a hydra head using the interactive terminal client.
