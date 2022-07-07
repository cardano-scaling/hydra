---
sidebar_position: 2
---

# Dockerで使用

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

> Hydra Headプロトコルのデモを行うための標準的なデモセットアップです。

デモの構成:

- 互いに直接接続された3つのHydraノードからなるクラスタで、それぞれが3つのHydraクレデンシャル `alice`, `bob`, `carol` のいずれかにアクセス
- ローカル開発ネットとして稼働する単一のブロック生成カルダノノード
- メトリクス収集用Prometheusサーバー
- 個々のHydraノードと対話するためのアドホック端末ユーザーインターフェイスクライアント

:::caution Caution!
genesisブロックから始まるアドホックなプライベート開発ネットを使用するため、開発ネットの設定が最新であることを都度確認する必要があります。 Cardano ノードから `LedgerNoView` エラーが発生した場合、開始時刻が過去になっているため、 `prepare-devnet.sh` スクリプトを使って更新する必要があります。
:::

## ネットワークの設定

この項では [Docker](https://www.docker.com/get-started) と [compose](https://www.docker.com/get-started) を使ってデモを実行します。高度な方法で実行したい場合は [Dockerを使用しないデモの実行](/docs/getting-started/demo/without-docker) へ移動してください。

:::info Context
以下のコマンドはすべて、プロジェクトリポジトリの `demo` フォルダから実行されるものとして書かれています。したがって、何かをする前にリポジトリをクローンして `cd demo` を実行して該当ディレクトリにポインターを合わせてください。
:::

:::warning OS Compatibility
この手順は、Linux環境（Ubuntu、NixOS）のみで検証しています。WindowsやMac OS Xの場合は、[Volumes](https://docs.docker.com/storage/volumes/)を使用するする必要があるかもしれません。
:::

まずは、composeファイルに定義されているサービスに必要なイメージを取得しましょう。

```mdx-code-block
<TerminalWindow>
docker-compose --profile tui pull
</TerminalWindow>
```

ここから、`./prepare-devnet.sh`スクリプトを実行して、開発用ネットワークの初期設定を作成することができます。 これは、Cardanoブロックチェーンを起動するために必要なジェネシスファイルを作成します。なお、今回のデモでは、ステークプールを一切必要としないシンプルな構成を使用しています。

```mdx-code-block
<TerminalWindow>
./prepare-devnet.sh
</TerminalWindow>
```

前置きは以上です。これで、ネットワークを立ち上げることができます。

```mdx-code-block
<TerminalWindow>
docker-compose up -d
</TerminalWindow>
```

便宜上、上記の手順をまとめたスクリプト `./run-docker.sh` を用意しており、サニティーチェックを行うことができます。

## ネットワークの構築

現在の開発段階では、HydraノードはHeadプロトコルを駆動するために特別に作られたUTXOのセット（「燃料」）と、HeadにコミットするためのUTXOが必要です。
同梱スクリプトの `./seed-devnet.sh` は、すでに実行中の `cardano-node` コンテナにある `cardano-cli` を使って、アリス、ボブ、キャロルにコミットする いくつかのUTXO エントリと燃料の UTXO を渡します。

:::info
これらのトランザクションは特別なものではないので、他のどのCardanoクライアントでも作成できます。ただし、以下のような特徴が必要です。
hydra-node実行ファイルの引数 `--cardano-signing-key` で定義された、Hydra Nodeの内部ウォレットで使用されるキーにコミットするために出力を支払う必要があります。
Outputの1つは、データムハッシュ `a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3` を含む必要があります。これは「燃料」マーカーであるからです。
:::

## クライアントの実行

compose を使うと、Hydra ノードと対話するためのデモ用ターミナルベースユーザーインターフェイス (別名 `hydra-tui`) を起動することができます。 composeの定義には、`hydra-tui-1`, `hydra-tui-2`, `hydra-tui-3` という3つのTUIサービスがあらかじめ設定されています。ターミナルで最初のHydraノードに接続するために、以下のコマンドを実行します。

```mdx-code-block
<TerminalWindow>
docker-compose --profile tui run hydra-tui-1
</TerminalWindow>
```

これは、最初のHydraノードに対応する署名キーをロードした本格的なターミナルインタフェースを開始するものです。他の端末では、`hydra-tui-2`と`hydra-tui-3`のサービスをターゲットとして、同様の方法で他のノードを起動することができます。