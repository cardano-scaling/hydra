# How to use etcd network

We will use offline mode, so lets prepare a starting utxo:

```json
cat << EOF > utxo.json
{
  "0000000000000000000000000000000000000000000000000000000000000000#0": {
    "address": "addr_test1vqg9ywrpx6e50uam03nlu0ewunh3yrscxmjayurmkp52lfskgkq5k",
    "value": {
      "lovelace": 100000000
    }
  }
}
EOF
```

### Start alice

Only showing etcd cluster output:

```shell
cabal exec hydra-node -- \
  --initial-utxo utxo.json \
  --ledger-protocol-parameters hydra-cluster/config/protocol-parameters.json \
  --persistence-dir ./alice \
  --node-id alice \
  --hydra-signing-key demo/alice.sk \
  --api-port 4001 \
  --port 5001 \
  --peer 127.0.0.1:5002 | grep etcd
```

### Start bob

Only showing etcd cluster output:

```shell
cabal exec hydra-node -- \
  --initial-utxo utxo.json \
  --ledger-protocol-parameters hydra-cluster/config/protocol-parameters.json \
  --persistence-dir ./bob \
  --node-id bob \
  --hydra-signing-key demo/bob.sk \
  --api-port 4002 \
  --port 5002 \
  --peer 127.0.0.1:5001 | grep etcd
```


### Query member list

Using alice's etcd node:

```shell
etcdctl member list
```
