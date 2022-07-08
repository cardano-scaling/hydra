---
sidebar_label: '基本的なHydra Head'
sidebar_position: 2
---

# 基本的なHydra Head

:hammer_and_wrench:  この資料は、現在作成中のものです。

このドキュメントでは、基本的なHydraHeadの展開アーキテクチャについて説明します。これは、この章で説明されている他のトポロジへの参照として機能し、以下に概略的に示されています。

```mdx-code-block
<p align="center">
  <img
    src={require('./basic-hydra-head.jpg').default}
    alt="Basic Hydra Head"
    height={400}
  />
</p>
```

Hydra Headの基本的なセットアップはいくつかの`hydra-node`で構成されており、それぞれが`caradano-node`（写真にはありません）を介してCardanoネットワークに接続されています。Hydra クライアント (`hydra-tui` など) は、オフチェーンネットワークを使用して Hydra Head を開くために、通常はローカル接続で `hydra-node` に接続することになります。 この画像は、2つの異なる`Hydraノード`の間に開かれた2つのヒドラヘッド（青と緑）を示しており、線はHydraネットワーク接続を示し、円は _HydraHead_ の状態と資格情報を表します。いわゆる`HydraHeadParty`によって要約されます。

画像には示されていませんが、同じ`hydra-node`で開いている複数の論理`Hydra Head`があります。これは最終的にサポートされ、`hydra-node`プロセス間のネットワーク接続の再利用を可能にする可能性が非常に高いです。

青または緑の各ヘッドは独立して進行でき、各ヘッドのそれぞれのハイドラパーティすべての署名が必要です。つまり、緑色のヘッドに2つの署名、青色のヘッドに4つの署名があります。
