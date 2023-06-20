---
sidebar_position: 2
---

# Docker で使用

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

この項では [Docker](https://www.docker.com/get-started) と [compose](https://www.docker.com/get-started) を使ってデモを実行します。高度な方法で実行したい場合は [Docker を使用しないデモの実行](/docs/getting-started/demo/without-docker) へ移動してください。

:::info Shortcut
便宜上、上記の手順をまとめたスクリプト `./run-docker.sh` を用意しており、サニティーチェックを行うことができます。
:::

:::info Context
以下のコマンドはすべて、プロジェクトリポジトリの `demo` フォルダから実行されるものとして書かれています。したがって、何かをする前にリポジトリをクローンして `cd demo` を実行して該当ディレクトリにポインターを合わせてください。
:::

:::warning OS Compatibility
この手順は、Linux 環境（Ubuntu、NixOS）のみで検証しています。Windows や Mac OS X の場合は、[Volumes](https://docs.docker.com/storage/volumes/)を使用するする必要があるかもしれません。
:::

## ネットワークの設定

まずは、compose ファイルに定義されているサービスに必要なイメージを取得しましょう。

```mdx-code-block
<TerminalWindow>
docker-compose --profile tui --profile hydra-node pull
</TerminalWindow>
```

ここから、`./prepare-devnet.sh`スクリプトを実行して、開発用ネットワークの初期設定を作成することができます。 これは、Cardano ブロックチェーンを起動するために必要なジェネシスファイルを作成します。なお、今回のデモでは、ステークプールを一切必要としないシンプルな構成を使用しています。

```mdx-code-block
<TerminalWindow>
./prepare-devnet.sh
</TerminalWindow>
```

前置きは以上です。これで、ネットワークを立ち上げることができます。

```mdx-code-block
<TerminalWindow>
docker-compose up -d cardano-node
</TerminalWindow>
```

:::caution Caution!
genesis ブロックから始まるアドホックなプライベート開発ネットを使用するため、開発ネットの設定が最新であることを都度確認する必要があります。 Cardano ノードから `TraceNoLedgerView` エラーが発生した場合、開始時刻が過去になっているため、 `prepare-devnet.sh` スクリプトを使って更新する必要があります。
:::

`docker-compose logs cardano-node -f` を使用してログをチェックすることで、ノードが稼働中であることを確認できます。 `TraceAdoptedBlock`を含むトレースが表示されるはずです。これは、devnet がブロックを生成していることを意味します。

## ネットワークの構築

同梱スクリプトの `./seed-devnet.sh` は、すでに実行中の `cardano-node` コンテナにある `cardano-cli` を使って、アリス、ボブ、キャロルにコミットする いくつかの UTXO エントリと燃料の UTXO を渡します。

```mdx-code-block
<TerminalWindow>
./seed-devnet.sh
</TerminalWindow>
```

## Hydra ノードの開始

最後に、オンチェーンの準備が整ったので、次のコマンドを実行して Hydra ネットワーク (つまり、アリス、ボブ、キャロルの 3 つのノードすべて) を立ち上げることができます。

```mdx-code-block
<TerminalWindow>
docker-compose --profile hydra-node up -d
</TerminalWindow>
```

## クライアントの実行

compose を使うと、Hydra ノードと対話するためのデモ用ターミナルベースユーザーインターフェイス (別名 `hydra-tui`) を起動することができます。 compose の定義には、`hydra-tui-1`, `hydra-tui-2`, `hydra-tui-3` という 3 つの TUI サービスがあらかじめ設定されています。ターミナルで最初の Hydra ノードに接続するために、以下のコマンドを実行します。

```mdx-code-block
<TerminalWindow>
docker-compose --profile tui run hydra-tui-1
</TerminalWindow>
```

これは、最初の Hydra ノードに対応する署名キーをロードした本格的なターミナルインタフェースを開始するものです。他の端末では、`hydra-tui-2`と`hydra-tui-3`のサービスをターゲットとして、同様の方法で他のノードを起動することができます。
