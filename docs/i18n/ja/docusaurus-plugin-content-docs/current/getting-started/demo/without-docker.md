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

## 準備

スコープに「cardano-node」、「hydra-node」、および「hydra-tui」実行可能ファイルがあることを確認してください。 次のいずれかを実行できます

 - `nix-shell demo` を使用するか、
 - `cabal build` と `cabal exec` を実行します (さらに引数を渡す前に `--` を忘れないでください)。

:::info tmux ユーザー向けのヒント
`demo` nix-shell には、以下のすべてのコマンドを実行する複数のウィンドウとペインで新しい `tmux` セッションを開始する `run-hydra-demo` スクリプトがあります!
:::

以降のすべてのコマンドは、プロジェクト リポジトリの `demo` フォルダから実行されるかのように記述されるため、続行する前に必ず `cd demo` を実行してください。

:::info nix-direnv ユーザー向けのヒント
`demo/.envrc` を許可すると、`demo/` ディレクトリにいるときはいつでも nix シェル環境を利用できるようになります。 これを使用するには、`cd demo` の後に `direnv allow` でオプトインします。
:::

## ネットワークの設定

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
  --socket-path node.socket \
  --shelley-operational-certificate opcert.cert \
  --shelley-kes-key kes.skey \
  --shelley-vrf-key vrf.skey
```

</TerminalWindow>
````

## Seeding The Network

You can use the `seed-devnet.sh` script by passing it the path/command to a cardano-cli and hydra-node executable to use, instead of having it using the Docker container. For example:


<TerminalWindow>

```
export CARDANO_NODE_SOCKET_PATH=devnet/node.socket
./seed-devnet.sh $(which cardano-cli) $(which hydra-node)"
```

</TerminalWindow>

Note, should you want to use `cabal`, pass the invocation for example like this `"cabal exec hydra-node --"`.

## Setting-up The Hydra Network

次に、3つの異なる端末で、`demo/` ディレクトリから3つの Hydra ノードを起動します。

````mdx-code-block
<Tabs>

<TabItem value="Alice">
<TerminalWindow>

```
source .env && hydra-node \
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
source .env && hydra-node \
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
source .env && hydra-node \
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
