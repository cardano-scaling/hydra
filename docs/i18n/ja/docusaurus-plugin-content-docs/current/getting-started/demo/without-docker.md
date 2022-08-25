---
sidebar_position: 3
---

# 実行ファイルで使用(Dockerなし)

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';
```

> Dockerコンテナなしで、実行ファイルとスクリプトでデモを実行します。

:::info Context
以下のコマンドはすべて、プロジェクトリポジトリの `demo` フォルダから実行されるものとして書かれています。したがって、何かをする前にリポジトリをクローンして `cd demo` を実行して該当ディレクトリにポインターを合わせてください。
:::

## ネットワークの設定

手動で `cardano-node` (開発ネット) と `hydra-node` を用意する必要があります。この手順では、`cabal exec` をビルドし、スコープ内にあることを前提としています。

まず、単一の `cardano-node` 開発ネット を用意し、この設定を使って起動します。カレントディレクトリに `devnet` ディレクトリが作成されることに注意してください。

````mdx-code-block
<TerminalWindow>

```
./prepare-devnet.sh
cd devnet
mkdir ipc
cabal exec cardano-node -- run \
  --config cardano-node.json \
  --topology topology.json \
  --database-path db \
  --socket-path ipc/node.socket \
  --shelley-operational-certificate credentials/stake-pool-1/opcert.cert \
  --shelley-kes-key credentials/stake-pool-1/kes.skey \
  --shelley-vrf-key credentials/stake-pool-1/vrf.skey
```

</TerminalWindow>
````

次に、3つの異なる端末で、`demo/` ディレクトリから3つの Hydra ノードを起動します。

````mdx-code-block
<Tabs>

<TabItem value="Alice">
<TerminalWindow>

```
cabal exec hydra-node -- \
  --node-id 1 --port 5001 --api-port 4001 --monitoring-port 6001 \
  --peer localhost:5002 \
  --peer localhost:5003 \
  --hydra-signing-key alice.sk \
  --hydra-verification-key bob.vk \
  --hydra-verification-key carol.vk \
  --cardano-signing-key devnet/credentials/alice.sk \
  --cardano-verification-key devnet/credentials/bob.vk \
  --cardano-verification-key devnet/credentials/carol.vk \
  --ledger-genesis devnet/genesis-shelley.json \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --network-id 42 \
  --node-socket devnet/node.socket
```

</TerminalWindow>
</TabItem>

<TabItem value="Bob">
<TerminalWindow>

```
cabal exec hydra-node -- \
  --node-id 2 --port 5002 --api-port 4002 --monitoring-port 6002 \
  --peer localhost:5001 \
  --peer localhost:5003 \
  --hydra-signing-key bob.sk \
  --hydra-verification-key alice.vk \
  --hydra-verification-key carol.vk \
  --cardano-signing-key devnet/credentials/bob.sk \
  --cardano-verification-key devnet/credentials/alice.vk \
  --cardano-verification-key devnet/credentials/carol.vk \
  --ledger-genesis devnet/genesis-shelley.json \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --network-id 42 \
  --node-socket devnet/node.socket
```

</TerminalWindow>
</TabItem>

<TabItem value="Carol">
<TerminalWindow>

```
cabal exec hydra-node -- \
  --node-id 3 --port 5003 --api-port 4003 --monitoring-port 6003 \
  --peer localhost:5001 \
  --peer localhost:5002 \
  --hydra-signing-key carol.sk \
  --hydra-verification-key alice.vk \
  --hydra-verification-key bob.vk \
  --cardano-signing-key devnet/credentials/carol.sk \
  --cardano-verification-key devnet/credentials/alice.vk \
  --cardano-verification-key devnet/credentials/bob.vk \
  --ledger-genesis devnet/genesis-shelley.json \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --network-id 42 \
  --node-socket devnet/node.socket
```

</TerminalWindow>
</TabItem>


</Tabs>
````

うまくいけば、チェーンに接続されたノードはログ収集を開始します。

## ネットワークの構築

Dockerコンテナを使用する代わりに、使用するcardano-cli実行可能ファイルへのパスを渡すことで、`seed-devnet.sh`スクリプトを使用できます。 例えば：


```mdx-code-block
<TerminalWindow>
./seed-devnet.sh $(which cardano-cli)
</TerminalWindow>
```

## クライアントの実行
hydra-tuiを使ってノードに接続します。例えば、アリスのハイドラノードと彼女のオンチェーンクレデンシャルを使用する場合。

````mdx-code-block
<TerminalWindow>

```
cabal exec hydra-tui -- \
  --connect 0.0.0.0:4001 \
  --cardano-signing-key devnet/credentials/alice.sk \
  --network-id 42 \
  --node-socket devnet/node.socket
```

</TerminalWindow>
````

ポート `4001` を `4002` または `4003` に置き換えて他の2ノードに接続し、 `alice.sk` をそれぞれ `bob.sk` または `carol.sk` に置き換えてください。
