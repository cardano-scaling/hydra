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

`hydra-node` の構成全体は、コマンドライン オプションを使用して提供されます。 オプションは、ネットワーク、API、チェーン接続、および使用される台帳のさまざまな要素を構成するために使用されます。 `--help` オプションを使用して、すべてのオプションの説明を取得できます。

```
hydra-node - A prototype of Hydra Head protocol

Usage: hydra-node ([-q|--quiet] (-n|--node-id NODE-ID) [-h|--host IP] 
                    [-p|--port PORT] [-P|--peer ARG] [--api-host IP] 
                    [--api-port PORT] [--monitoring-port PORT] 
                    [--hydra-signing-key FILE] [--hydra-verification-key FILE]
                    --hydra-scripts-tx-id TXID [--persistence-dir DIR] 
                    [--testnet-magic INTEGER] [--node-socket FILE] 
                    [--cardano-signing-key FILE] 
                    [--cardano-verification-key FILE] 
                    [--start-chain-from SLOT.HEADER_HASH] 
                    [--ledger-genesis FILE] 
                    [--ledger-protocol-parameters FILE] |
                    COMMAND) [--version] [--script-info]

  Starts a Hydra Node

Available options:
  -q,--quiet               Turns off logging.
  -n,--node-id NODE-ID     The Hydra node identifier used on the Hydra network.
                           It is important to have a unique identifier in order
                           to be able distinguish between connected peers.
  -h,--host IP             Listen address for incoming Hydra network
                           connections. (default: 0.0.0.0)
  -p,--port PORT           Listen port for incoming Hydra network connections.
                           (default: 5001)
  -P,--peer ARG            A peer address in the form <host>:<port>, where
                           <host> can be an IP address, or a host name. Can be
                           provided multiple times, once for each peer node.
  --api-host IP            Listen address for incoming client API connections.
                           (default: 127.0.0.1)
  --api-port PORT          Listen port for incoming client API connections.
                           (default: 4001)
  --monitoring-port PORT   Listen port for monitoring and metrics via
                           prometheus. If left empty, monitoring server is not
                           started.
  --hydra-signing-key FILE Hydra signing key used by our hydra-node.
                           (default: "hydra.sk")
  --hydra-verification-key FILE
                           Hydra verification key of another party in the Head.
                           Can be provided multiple times, once for each
                           participant.
  --hydra-scripts-tx-id TXID
                           The transaction which is expected to have published
                           Hydra scripts as reference scripts in its outputs.
                           Note: All scripts need to be in the first 10 outputs.
                           See release notes for pre-published versions. You can
                           use the 'publish-scripts' sub-command to publish them
                           yourself.
  --persistence-dir DIR    The directory where the Hydra Head state is stored.Do
                           not edit these files manually!
  --testnet-magic INTEGER     Network identifier for a testnet to connect to. We
                           only need to provide the magic number here. For
                           example: '2' is the 'preview' network. See
                           https://book.world.dev.cardano.org/environments.html
                           for available networks. (default: 42)
  --node-socket FILE       Filepath to local unix domain socket used to
                           communicate with the cardano node.
                           (default: "node.socket")
  --cardano-signing-key FILE
                           Cardano signing key of our hydra-node. This will be
                           used to 'fuel' and sign Hydra protocol transactions,
                           as well as commit UTxOs from. (default: "cardano.sk")
  --cardano-verification-key FILE
                           Cardano verification key of another party in the
                           Head. Can be provided multiple times, once for each
                           participant.
  --start-chain-from SLOT.HEADER_HASH
                           The id of the block we want to start observing the
                           chain from. If not given, uses the chain tip at
                           startup. Composed by the slot number, a separator
                           ('.') and the hash of the block header. For example:
                           52970883.d36a9936ae7a07f5f4bdc9ad0b23761cb7b14f35007e54947e27a1510f897f04.
  --ledger-genesis FILE    Path to a Shelley-compatible genesis JSON file used
                           for the Hydra ledger. You can use the corresponding
                           Cardano network's shelley genesis file from:
                           https://book.world.dev.cardano.org/environments.html
                           (default: "genesis-shelley.json")
  --ledger-protocol-parameters FILE
                           Path to protocol parameters used in the Hydra Head.
                           See manual how to configure this.
                           (default: "protocol-parameters.json")
  --version                Show version
  --script-info            Dump script info as JSON
  -h,--help                Show this help text

Available commands:
  publish-scripts          Publish Hydra's Plutus scripts on chain to be used
                           by the hydra-node as --hydra-script-tx-id.
                           
                            ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓ 
                            ┃              ⚠ WARNING ⚠              ┃ 
                            ┣═══════════════════════════════════════┫ 
                            ┃    This costs money. About 50 Ada.    ┃ 
                            ┃ Spent using the provided signing key. ┃ 
                            ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ 
```

:::info  Dynamic Configuration

現在のコマンドラインは、ユーザーフレンドリーとは言い難く、大規模なクラスタのセットアップにはやや使いにくいということは認識しています。

しかし、より使いやすく、動的に設定できるようにする計画があります。[#240](https://github.com/input-output-hk/hydra/issues/240) & [ADR-15](/adr/15) を参照してください。
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

2つ目のキーセットは、いわゆるHydraキーで、Head内のスナップショットのマルチシグネチャに使用されるものです。長期的には、これらの鍵はMuSig2アグリゲーション・マルチシグネチャ方式で使用されるキーペアとなる予定です。しかし、現時点では、集約型マルチシグネチャ暗号は[未実装](https://github.com/input-output-hk/hydra/issues/193)で、HydraノードはEd25519鍵に基づく安全なマルチシグネチャ方式となります。

これらはカルダノキーに似ていますが、混同しないでください。したがって、基本的にキーマテリアルで直接構成される別の基本的なディスク上の表現を使用します（カルダノキーは通常、テキストエンベロープにCBORエンコードされて保存されます）。デモ用の鍵ペアは、 `alice.{vk,sk}`、 `bob.{vk,sk}` 、 `carol.{vk,sk}` を [demo folder](https://github.com/input-output-hk/hydra/tree/master/demo)に用意しています。 現在、参加者はその中から1つを選び、Cardanoの鍵と同様の方法で、検証鍵を仲間と共有し、署名鍵を彼らに使うことが期待されている。 (TODO: システムのエントロピーを利用して新しいものを生成する簡単な方法を提供すべきである)

### 元帳パラメーター

Hydra-Headのコアには、台帳があります。現時点では、HydraはCardanoにのみ配線されており、レイヤー1で使われているものと同様の台帳構成を想定しています。 これは、2つのコマンドラインオプション `--ledger-genesis` と `--ledger-protocol-parameters` として翻訳されます。前者は（シェリー！）生成規則を定義し、より具体的には、台帳が必要とする**グローバル**で更新不可能なプロトコルパラメータを定義しています。後者は、手数料や取引サイズなど、更新可能なプロトコルパラメータを定義します。これらはcardano-cliで使用されるものと同じ形式を使用します（例：`cardano-cli query protocol-parameters`の出力）。

[hydra-cluster/config](https://github.com/input-output-hk/hydra/blob/master/hydra-cluster/config)に既存のファイルを提供しており、これをベースに利用することができます。特に、ヘッド内のコストを無効化するためのプロトコルパラメータが定義されています。それとは別に、現在のメインネットのパラメータもそのままコピーしています。ヒドラの台帳の面白いところは、レイヤー1と同じルールやコードを再利用している（いわゆる同型）にもかかわらず、パラメーターがレイヤー1とは若干異なるように変更されている点です。これは料金の場合ですが、例えばスクリプトの最大実行予算などでも可能です。ただし、すべてのパラメータが安全に変更できるわけではありません。値の最大サイズ（ネイティブアセットを運ぶ）を制御するパラメータや、UTxOの最小Ada値を変更すると、ヘッドが「closable」になってしまう可能性があります。経験則から言うと、取引に厳密に適用されるもの（手数料、実行単位、最大TXサイズ...）は変更しても安全である。しかし、UTxOに反映される可能性があるものは、そうではありません。 

:::info About Protocol Parameters
ほとんどのプロトコルパラメータは、まず第一にGenesisパラメータであるため、2つのファイルの間に少し重複があることに注意してください。さらに、これらのパラメーターの多くは、Hydraのコンテキストでは実際には無関係です（たとえば、Headの中に金庫やステークプールがないため、報酬インセンティブまたは委任ルールを構成するパラメーターは使用できません）。
:::

### 燃料

最後に、Hydraノードがすべて動作するために必要なもう1つのことは、内部ウォレットについてです。実際、Hydraノードには現在初歩的なウォレットが付属しており、Headライフサイクル（Init, Commit, Close, Fanout...）を駆動するトランザクションの燃料として利用されています。これらのトランザクションはレイヤー1で発生するので、お金がかかります。

今のところ、これはHydraのウォレットによって内部的に管理されていますが、いくつかの助けが必要です。ノードに提供されるCardanoキーは、資金を保持することが期待されています。具体的には、特定のデータムハッシュでマークされた、少なくとも1つのUTxOエントリーがあります。

```bash title="Fuel datum hash"
a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3
```

便利なことに（少なくとも、今できる限り）、cardano-cliを使用して通常のUTxOをマークされた燃料UTxOに変換する[create-marker-utxo.sh](https://github.com/input-output-hk/hydra/blob/master/sample-node-config/gcp/scripts/create-marker-utxo.sh)というスクリプトが用意されています。マーカーが必要な理由は、Cardanoの鍵はコミットに必要な資金も保持することが期待されるからです（ただし、マークされていません）

:::info About commits
長期的には、私たちは[Hydraノード外のコミットを移動](https://github.com/input-output-hk/hydra/issues/215)して、外部のウォレット（おそらく[CIP-0030](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030)標準に従うウォレットを通じて）で行われるようにします。
:::

## セットアップ例

### GoogleクラウドとTerraform

クラウド上の仮想マシン上でHydraノードをホストするためのサンプルノード構成を[sample-node-config/](https://github.com/input-output-hk/hydra/tree/master/sample-node-config/gcp/)ディレクトリに提供しています。 特に、このセットアップには [docker-compose.yaml](https://github.com/input-output-hk/hydra/blob/master/sample-node-config/gcp/docker-compose.yaml) という仕様があり、cardano-node + hydra-node サービスを設定するための良いテンプレートが提供されています。また、クラスタをセットアップするための様々な便利なスクリプトも提供されています。
