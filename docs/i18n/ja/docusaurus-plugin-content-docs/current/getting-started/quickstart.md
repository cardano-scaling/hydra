---
sidebar_position: 3
---

# クイックスタート

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

> `hydra-node`を使用した最初のステップ。

Hydra ヘッドを動かすということは、他の複数のHydraノードに接続され、Cardanoノードに接続されたHydraノードを動かすということです。したがって、 [cardano-node](https://github.com/input-output-hk/cardano-node/) の実行は Hydraヘッドを動かすための前提条件となります。このガイドでは、Cardanoノードの実行に関する詳細については説明しませんので、必要であれば、この件に関する既存のドキュメントを探してください。

:::tip cardano-node & cardano-cli
Cardanoノードの実行には、コンテナや[公式Dockerイメージ](https://hub.docker.com/r/inputoutput/cardano-node)の使用を推奨しています。  

このイメージには `cardano-node` と `cardano-cli` の両方が含まれています。`cardano-cli`は様々なコマンドを実行するのに便利で、例えばアドレスの作成や鍵の生成を行うことができます。
:::

## Hydra-nodeのオプション...

現在`hydra-node` コマンドラインはノードを起動するための単一のコマンドのみを提供しています。設定全体はコマンドラインオプションで提供され、完全に静的です。オプションは様々な要素を設定するために使用され、以下のようにまとめることができます (ただし、これらのオプションのリファレンスドキュメントは `--help` フラグの下に用意されていることに注意してください)。

オプション                                                 | 概要
---                                                     | ---
`--node-id`                                             | Hydraノードの識別子で、Headネットワーク内の識別子として機能します。
`--peer`                                                | Hydraネットワークのピアのアドレス。各ピアごとに複数回指定する必要があります。
`--host` <br/> `--port`                                 | このHydraノードのホストとポート。Hydraネットワークからのピア接続先
`--node-socket`                                         | CardanoノードのIPCソケットのファイルパス。ノードとのプロセス間通信に使用されます。
`--ledger-genesis` <br/> `--ledger-protocol-parameters` | ハイドラ台帳のルールとヘッドのパラメータ。
`--hydra-signing-key` <br/> `--cardano-signing-key` <br/> `--hydra-verification-key` <br/> `--cardano-verification-key` | CardanoとHydraの認証情報。 これらのオプションは、ピアの数に応じて複数回指定される場合もあります。

任意オプション:

オプション                        | 概要
---                             | ---
`--api-host` <br/> `--api-port` | [WebSocket API](/api-reference)と対話するためのHydra API のホストとポート。
`--monitoring-port`             | Prometheusによるモニタリングとメトリクスのためのポート。空白の場合、監視サーバは起動しません。

:::info  Dynamic Configuration

現在のコマンドラインは、ユーザーフレンドリーとは言い難く、大規模なクラスタのセットアップにはやや使いにくいということは認識しています。

しかし、より使いやすく、動的に設定できるようにする計画があります。[#240](https://github.com/input-output-hk/hydra-poc/issues/240) & [ADR-15](/adr/15) を参照してください。
:::

## 詳細について

### カルダノ鍵(キー)

前の項目では、Hydraノードのセットアップに必要なさまざまなオプションと要素について説明しました。この項目では、取得方法についてその一部をご紹介します。まず、Cardanoキーファイル(`--cardano-signing-key` と `--cardano-verification-key`)からです。

ヘッド内において、参加者は2組の鍵で認証されます。 1組の鍵ペアは、Cardanoですでに一般的なEd25519の公開鍵/秘密鍵ペアです。このような鍵ペアは `cardano-cli` を用いて以下のように生成することができます。

```mdx-code-block
<TerminalWindow>
cardano-cli address key-gen --verification-key-file cardano.vk --signing-key-file cardano.sk
</TerminalWindow>
```

各参加者は自分の検証キーを他の参加者と共有することになります。 ノードを起動するには、**自分の署名キー**と**他の参加者の検証キー**が必要です。これらの鍵は現在、Hydraプロトコルの実行を促すオンチェーン・トランザクションの認証に使用されています。これは、望まれないアクターがヘッドのライフサイクルをいじくり回すのを防ぐためです（たとえば、ヘッドの外部の誰かが、初期化されたヘッドを中止させることができるのです）。これはヘッド参加者の資金を危険にさらすものではありませんが、それでも防ぎたい厄介なものです。

### Hydra鍵(キー)

2つ目のキーセットは、いわゆるHydraキーで、Head内のスナップショットのマルチシグネチャに使用されるものです。長期的には、これらの鍵はMuSig2アグリゲーション・マルチシグネチャ方式で使用されるキーペアとなる予定です。しかし、現時点では、集約型マルチシグネチャ暗号は[未実装](https://github.com/input-output-hk/hydra-poc/issues/193)で、HydraノードはEd25519鍵に基づく安全なマルチシグネチャ方式となります。

これらはカルダノキーに似ていますが、混同しないでください。したがって、基本的にキーマテリアルで直接構成される別の基本的なディスク上の表現を使用します（カルダノキーは通常、テキストエンベロープにCBORエンコードされて保存されます）。デモ用の鍵ペアは、 `alice.{vk,sk}`、 `bob.{vk,sk}` 、 `carol.{vk,sk}` を [demo folder](https://github.com/input-output-hk/hydra-poc/tree/master/demo)に用意しています。 現在、参加者はその中から1つを選び、Cardanoの鍵と同様の方法で、検証鍵を仲間と共有し、署名鍵を彼らに使うことが期待されている。 (TODO: システムのエントロピーを利用して新しいものを生成する簡単な方法を提供すべきである)

### 元帳パラメーター

Hydra-Headのコアには、台帳があります。現時点では、HydraはCardanoにのみ配線されており、レイヤー1で使われているものと同様の台帳構成を想定しています。 これは、2つのコマンドラインオプション `--ledger-genesis` と `--ledger-protocol-parameters` として翻訳されます。前者は（シェリー！）生成規則を定義し、より具体的には、台帳が必要とする**グローバル**で更新不可能なプロトコルパラメータを定義しています。後者は、手数料や取引サイズなど、更新可能なプロトコルパラメータを定義します。これらはcardano-cliで使用されるものと同じ形式を使用します（例：`cardano-cli query protocol-parameters`の出力）。

[hydra-cluster/config](https://github.com/input-output-hk/hydra-poc/blob/master/hydra-cluster/config)に既存のファイルを提供しており、これをベースに利用することができます。特に、ヘッド内のコストを無効化するためのプロトコルパラメータが定義されています。それとは別に、現在のメインネットのパラメータもそのままコピーしています。ヒドラの台帳の面白いところは、レイヤー1と同じルールやコードを再利用している（いわゆる同型）にもかかわらず、パラメーターがレイヤー1とは若干異なるように変更されている点です。これは料金の場合ですが、例えばスクリプトの最大実行予算などでも可能です。ただし、すべてのパラメータが安全に変更できるわけではありません。値の最大サイズ（ネイティブアセットを運ぶ）を制御するパラメータや、UTxOの最小Ada値を変更すると、ヘッドが「closable」になってしまう可能性があります。経験則から言うと、取引に厳密に適用されるもの（手数料、実行単位、最大TXサイズ...）は変更しても安全である。しかし、UTxOに反映される可能性があるものは、そうではありません。 

:::info About Protocol Parameters
ほとんどのプロトコルパラメータは、まず第一にGenesisパラメータであるため、2つのファイルの間に少し重複があることに注意してください。さらに、これらのパラメーターの多くは、Hydraのコンテキストでは実際には無関係です（たとえば、Headの中に金庫やステークプールがないため、報酬インセンティブまたは委任ルールを構成するパラメーターは使用できません）。
:::

### 燃料

最後に、Hydraノードがすべて動作するために必要なもう1つのことは、内部ウォレットについてです。実際、Hydraノードには現在初歩的なウォレットが付属しており、Headライフサイクル（Init, Commit, Close, Fanout...）を駆動するトランザクションの燃料として利用されています。これらのトランザクションはレイヤー1で発生するので、お金がかかります。

今のところ、これはHydraのウォレットによって内部的に管理されていますが、いくつかの助けが必要です。ノードに提供されるCardanoキーは、資金を保持することが期待されています。具体的には、特定のデータムハッシュでマークされた、少なくとも1つのUTxOエントリーがあります。

```bash title="Fuel datum hash"
a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3
```

便利なことに（少なくとも、今できる限り）、cardano-cliを使用して通常のUTxOをマークされた燃料UTxOに変換する[create-marker-utxo.sh](https://github.com/input-output-hk/hydra-poc/blob/master/sample-node-config/gcp/scripts/create-marker-utxo.sh)というスクリプトが用意されています。マーカーが必要な理由は、Cardanoの鍵はコミットに必要な資金も保持することが期待されるからです（ただし、マークされていません）

:::info About commits
長期的には、私たちは[Hydraノード外のコミットを移動](https://github.com/input-output-hk/hydra-poc/issues/215)して、外部のウォレット（おそらく[CIP-0030](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030)標準に従うウォレットを通じて）で行われるようにします。
:::

## セットアップ例

### GoogleクラウドとTerraform

クラウド上の仮想マシン上でHydraノードをホストするためのサンプルノード構成を[sample-node-config/](https://github.com/input-output-hk/hydra-poc/tree/master/sample-node-config/gcp/)ディレクトリに提供しています。 特に、このセットアップには [docker-compose.yaml](https://github.com/input-output-hk/hydra-poc/blob/master/sample-node-config/gcp/docker-compose.yaml) という仕様があり、cardano-node + hydra-node サービスを設定するための良いテンプレートが提供されています。また、クラスタをセットアップするための様々な便利なスクリプトも提供されています。
