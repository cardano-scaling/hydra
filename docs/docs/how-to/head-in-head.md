# Open a head in a head

Start the L1 devnet and an L2 head:
```shell
nix run .#hydra-cluster -- --devnet --publish-hydra-scripts --state-directory inception-demo
```

Explain briefly the UTxO on the L1 (and keep open):
```shell
watch -n1 cardano-cli query utxo --whole-utxo --socket-path inception-demo/node.socket --testnet-magic 42
```

Show L2 utxo state and demonstrate 1-2 transactions:
```shell
nix run .#hydra-tui -- -c 0.0.0.0:4001 -k inception-demo/wallet.sk
```

Publish hydra scripts on L2:

```shell
mkdir -p inception-demo/l3
curl localhost:4001/protocol-parameters > inception-demo/l3/protocol-parameters.json

function publishScripts() {
  nix run .#hydra-node -- publish-scripts \
    --inception 0.0.0.0:4001 \
    --cardano-signing-key inception-demo/wallet.sk
}

export HYDRA_SCRIPTS_TX_ID_ON_L2="$(publishScripts)"
```

Prepare another key to separate fuel from funds:
```shell
mkdir -p inception-demo/l3
cardano-cli address key-gen --normal-key \
  --verification-key-file inception-demo/l3/wallet.vk \
  --signing-key-file inception-demo/l3/wallet.sk
  
echo "Send some funds to commit to L3 to:"
cardano-cli address build --testnet-magic 42 --payment-verification-key-file inception-demo/l3/wallet.vk
```

Use the TUI on the L2 to separate fuel from funds using the interactive `[n]ew transaction`.

Start a hydra-node on L2:

```shell
nix run .#hydra-node -- \
  --node-id "l3" \
  --api-port 4003 \
  --port 5003 \
  --inception 0.0.0.0:4001 \
  --hydra-scripts-tx-id ${HYDRA_SCRIPTS_TX_ID_ON_L2} \
  --persistence-dir inception-demo/l3 \
  --cardano-signing-key inception-demo/wallet.sk \
  --hydra-signing-key demo/bob.sk \
  --ledger-protocol-parameters inception-demo/l3/protocol-parameters.json
```

Open the head on L2 with the tui:
```shell
nix run .#hydra-tui -- -c 0.0.0.0:4003 -k inception-demo/wallet.sk
```

Commit via terminal (tui only supports direct node)

```shell
export WALLET_L3_ADDR=$(cardano-cli address build --testnet-magic 42 --payment-verification-key-file inception-demo/l3/wallet.vk)

cd inception-demo

curl 0.0.0.0:4001/snapshot/utxo \
  | jq "with_entries(select(.value.address == \"${WALLET_L3_ADDR}\"))" \
  > l2-commit-utxo.json

curl -X POST 0.0.0.0:4003/commit \
  --data @l2-commit-utxo.json \
  > commit-tx.json

cardano-cli transaction sign \
  --tx-file commit-tx.json \
  --signing-key-file l3/wallet.sk \
  --out-file commit-tx-signed.json

cat commit-tx-signed.json | jq -c '{tag: "NewTx", transaction: .}' | websocat "ws://0.0.0.0:4001?history=no"
```
