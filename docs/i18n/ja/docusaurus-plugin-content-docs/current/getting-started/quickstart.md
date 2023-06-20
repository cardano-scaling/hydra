---
sidebar_position: 3
---

# クイックスタート

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

> `hydra-node`を使用した最初のステップ。

Hydra ヘッドを動かすということは、他の複数の Hydra ノードに接続され、Cardano ノードに接続された Hydra ノードを動かすということです。したがって、 [cardano-node](https://github.com/input-output-hk/cardano-node/) の実行は Hydra ヘッドを動かすための前提条件となります。このガイドでは、Cardano ノードの実行に関する詳細については説明しませんので、必要であれば、この件に関する既存のドキュメントを探してください。

:::tip cardano-node & cardano-cli
Cardano ノードの実行には、コンテナや[公式 Docker イメージ](https://hub.docker.com/r/inputoutput/cardano-node)の使用を推奨しています。

このイメージには `cardano-node` と `cardano-cli` の両方が含まれています。`cardano-cli`は様々なコマンドを実行するのに便利で、例えばアドレスの作成や鍵の生成を行うことができます。
:::

## Hydra-node のオプション...

`hydra-node` の構成全体は、コマンドライン オプションを使用して提供されます。 オプションは、ネットワーク、API、チェーン接続、および使用される台帳のさまざまな要素を構成するために使用されます。 `--help` オプションを使用して、すべてのオプションの説明を取得できます。

```
hydra-node - Implementation of the Hydra Head protocol

Usage: hydra-node ([-q|--quiet] (-n|--node-id NODE-ID) [-h|--host IP]
                    [-p|--port PORT] [-P|--peer ARG] [--api-host IP]
                    [--api-port PORT] [--monitoring-port PORT]
                    [--hydra-signing-key FILE] [--hydra-verification-key FILE]
                    [--hydra-scripts-tx-id TXID] [--persistence-dir DIR]
                    [--mainnet | --testnet-magic NATURAL] [--node-socket FILE]
                    [--cardano-signing-key FILE]
                    [--cardano-verification-key FILE]
                    [--start-chain-from SLOT.HEADER_HASH]
                    [--contestation-period CONTESTATION-PERIOD]
                    [--ledger-protocol-parameters FILE] |
                    COMMAND) [--version] [--script-info]

  Starts a Hydra Node

Available options:
  -q,--quiet               Turns off logging.
  -n,--node-id NODE-ID     The Hydra node identifier used on the Hydra network.
                           It is important to have a unique identifier in order
                           to be able distinguish between connected peers.
  -h,--host IP             Listen address for incoming Hydra network
                           connections. (default: 127.0.0.1)
  -p,--port PORT           Listen port for incoming Hydra network connections.
                           (default: 5001)
  -P,--peer ARG            A peer address in the form <host>:<port>, where
                           <host> can be an IP address, or a host name. Can be
                           provided multiple times, once for each peer (current
                           maximum limit is 4 peers).
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
                           participant (current maximum limit is 4 ).
  --hydra-scripts-tx-id TXID
                           The transaction which is expected to have published
                           Hydra scripts as reference scripts in its outputs.
                           Note: All scripts need to be in the first 10 outputs.
                           See release notes for pre-published versions. You can
                           use the 'publish-scripts' sub-command to publish them
                           yourself.
  --persistence-dir DIR    The directory where the Hydra Head state is stored.Do
                           not edit these files manually!
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Network identifier for a testnet to connect to. We
                           only need to provide the magic number here. For
                           example: '2' is the 'preview' network. See
                           https://book.world.dev.cardano.org/environments.html
                           for available networks. (default: 42)
  --node-socket FILE       Filepath to local unix domain socket used to
                           communicate with the cardano node.
                           (default: "node.socket")
  --cardano-signing-key FILE
                           Cardano signing key of our hydra-node. This will be
                           used to authorize Hydra protocol transactions for
                           heads the node takes part in and any funds owned by
                           this key will be used as 'fuel'.
                           (default: "cardano.sk")
  --cardano-verification-key FILE
                           Cardano verification key of another party in the
                           Head. Can be provided multiple times, once for each
                           participant (current maximum limit is 4).
  --start-chain-from SLOT.HEADER_HASH
                           The id of the block we want to start observing the
                           chain from. If not given, uses the chain tip at
                           startup. Composed by the slot number, a separator
                           ('.') and the hash of the block header. For example:
                           52970883.d36a9936ae7a07f5f4bdc9ad0b23761cb7b14f35007e54947e27a1510f897f04.
  --contestation-period CONTESTATION-PERIOD
                           Contestation period for close transaction in seconds.
                           If this value is not in sync with other participants
                           hydra-node will ignore the initial tx. Additionally,
                           this value needs to make sense compared to the
                           current network we are running. (default: 60s)
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

:::info Dynamic Configuration

現在のコマンドラインは、ユーザーフレンドリーとは言い難く、大規模なクラスタのセットアップにはやや使いにくいということは認識しています。

しかし、より使いやすく、動的に設定できるようにする計画があります。[#240](https://github.com/input-output-hk/hydra/issues/240) & [ADR-15](/adr/15) を参照してください。
:::

## 詳細について

### カルダノ鍵(キー)

前の項目では、Hydra ノードのセットアップに必要なさまざまなオプションと要素について説明しました。この項目では、取得方法についてその一部をご紹介します。まず、Cardano キーファイル(`--cardano-signing-key` と `--cardano-verification-key`)からです。

ヘッド内において、参加者は 2 組の鍵で認証されます。 1 組の鍵ペアは、Cardano ですでに一般的な Ed25519 の公開鍵/秘密鍵ペアです。このような鍵ペアは `cardano-cli` を用いて以下のように生成することができます。

```mdx-code-block
<TerminalWindow>
cardano-cli address key-gen --verification-key-file cardano.vk --signing-key-file cardano.sk
</TerminalWindow>
```

各参加者は自分の検証キーを他の参加者と共有することになります。 ノードを起動するには、**自分の署名キー**と**他の参加者の検証キー**が必要です。これらの鍵は現在、Hydra プロトコルの実行を促すオンチェーン・トランザクションの認証に使用されています。これは、望まれないアクターがヘッドのライフサイクルをいじくり回すのを防ぐためです（たとえば、ヘッドの外部の誰かが、初期化されたヘッドを中止させることができるのです）。これはヘッド参加者の資金を危険にさらすものではありませんが、それでも防ぎたい厄介なものです。

### Hydra 鍵(キー)

2 つ目のキーセットは、いわゆる Hydra キーで、Head 内のスナップショットのマルチシグネチャに使用されるものです。長期的には、これらの鍵は MuSig2 アグリゲーション・マルチシグネチャ方式で使用されるキーペアとなる予定です。しかし、現時点では、集約型マルチシグネチャ暗号は[未実装](https://github.com/input-output-hk/hydra/issues/193)で、Hydra ノードは Ed25519 鍵に基づく安全なマルチシグネチャ方式となります。

これらはカルダノキーに似ていますが、混同しないでください。したがって、基本的にキーマテリアルで直接構成される別の基本的なディスク上の表現を使用します（カルダノキーは通常、テキストエンベロープに CBOR エンコードされて保存されます）。デモ用の鍵ペアは、 `alice.{vk,sk}`、 `bob.{vk,sk}` 、 `carol.{vk,sk}` を [demo folder](https://github.com/input-output-hk/hydra/tree/master/demo)に用意しています。 現在、参加者はその中から 1 つを選び、Cardano の鍵と同様の方法で、検証鍵を仲間と共有し、署名鍵を彼らに使うことが期待されている。 (TODO: システムのエントロピーを利用して新しいものを生成する簡単な方法を提供すべきである)

### 元帳パラメーター

ヒドラの頭の中核には台帳があります。 現時点では、Hydra は Cardano にのみ接続されており、レイヤー 1 で使用されているものと同様のレジャー構成を想定しています。これは、コマンドライン オプション `--ledger-protocol-parameters` として変換されます。 これにより、料金やトランザクション サイズなどの更新可能なプロトコル パラメーターが定義されます。 これらは、cardano-cli で使用される形式と同じ形式を使用します (例: `cardano-cli query protocol-parameters`の出力)。

[hydra-cluster/config](https://github.com/input-output-hk/hydra/blob/master/hydra-cluster/config)に既存のファイルを提供しており、これをベースに利用することができます。特に、ヘッド内のコストを無効化するためのプロトコルパラメータが定義されています。それとは別に、現在のメインネットのパラメータもそのままコピーしています。ヒドラの台帳の面白いところは、レイヤー 1 と同じルールやコードを再利用している（いわゆる同型）にもかかわらず、パラメーターがレイヤー 1 とは若干異なるように変更されている点です。これは料金の場合ですが、例えばスクリプトの最大実行予算などでも可能です。ただし、すべてのパラメータが安全に変更できるわけではありません。値の最大サイズ（ネイティブアセットを運ぶ）を制御するパラメータや、UTxO の最小 Ada 値を変更すると、ヘッドが「closable」になってしまう可能性があります。経験則から言うと、取引に厳密に適用されるもの（手数料、実行単位、最大 TX サイズ...）は変更しても安全である。しかし、UTxO に反映される可能性があるものは、そうではありません。

:::info About Protocol Parameters
ほとんどのプロトコルパラメータは、まず第一に Genesis パラメータであるため、2 つのファイルの間に少し重複があることに注意してください。さらに、これらのパラメーターの多くは、Hydra のコンテキストでは実際には無関係です（たとえば、Head の中に金庫やステークプールがないため、報酬インセンティブまたは委任ルールを構成するパラメーターは使用できません）。
:::

### Fuel

Finally, one last bit necessary to get Hydra nodes up and running is to fuel them up! All the transactions driving the Head lifecycle (Init, Commit, Close, ...) need to be submitted to the layer 1, and hence they cost money!

For that, any funds owned by the `--cardano-signing-key` given to the `--hydra-node` will be considered spendable to pay fees or use as collateral for these Hydra protocol transactions. Consequently, sending some ADA-only funds to the address of the this "internal wallet" is required. To get the address for the cardano keys as generated above, one can use for example the cardano-cli:

<TerminalWindow>

```sh
cardano-cli address build --verification-key-file cardano.vk --mainnet
# addr1v92l229athdj05l20ggnqz24p4ltlj55e7n4xplt2mxw8tqsehqnt
```

</TerminalWindow>

:::warning Old fuel
Marking fuel using datum hashes is not needed anymore as support for committing
directly from it will be removed in future Hydra versions.
Please take a look at [external-commits](/docs/getting-started/quickstart#external-commits).
:::

To distinguish fuel from outputs to be committable by the `hydra-node`, we used
to mark one output with a specific datum hash:

```sh title="Fuel datum hash"
a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3
```

To create such an output, we provide a [create-marker-utxo.sh](https://github.com/input-output-hk/hydra/blob/master/sample-node-config/gcp/scripts/create-marker-utxo.sh) script that uses the cardano-cli to convert a normal UTxO into a marked fuel UTxO.

For easy scripting purpose, `hydra-tools` provide a dedicated command to output the current marker datum hash:

<TerminalWindow>

```sh
hydra-tools marker-hash
# "a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3"
```

</TerminalWindow>

## External commits

While the `hydra-node` holds funds to fuel protocol transactions, any wallet can be used to commit funds into an `initializing` Hydra head. The `hydra-node` provides an HTTP endpoint at `/commit`, which allows to specify multiple UTxO (belonging to public key or script address) and returns a draft transaction. This transaction is already balanced and all fees are paid by the funds held by the `hydra-node`, but is missing witnesses for the public key outputs to commit. Hence, an integrated wallet would need to sign this transaction and submit it to the Cardano network. See the [api documentation](pathname:///api-reference/#operation-publish-/commit) for details.

## セットアップ例

### Google クラウドと Terraform

クラウド上の仮想マシン上で Hydra ノードをホストするためのサンプルノード構成を[sample-node-config/](https://github.com/input-output-hk/hydra/tree/master/sample-node-config/gcp/)ディレクトリに提供しています。 特に、このセットアップには [docker-compose.yaml](https://github.com/input-output-hk/hydra/blob/master/sample-node-config/gcp/docker-compose.yaml) という仕様があり、cardano-node + hydra-node サービスを設定するための良いテンプレートが提供されています。また、クラスタをセットアップするための様々な便利なスクリプトも提供されています。

## Running on Mainnet

Hydra node is compatible with the mainnet network. To choose this network you need to specify `--mainnet` flag for the network id in the hydra-node arguments. We publish the hydra scripts on each new release and you can find them on the [release page](https://github.com/input-output-hk/hydra/releases) (look for section _Hydra Scripts_).

Please be sure to read the [relevant section](/docs/known-issues) section to fully understand the limitations and consequences of running Hydra nodes on mainnet.
