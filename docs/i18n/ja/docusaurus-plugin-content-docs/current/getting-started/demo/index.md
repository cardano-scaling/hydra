---
sidebar_position: 1
---

# デモ

> Hydra Headプロトコルのデモを行うための標準的なデモセットアップです。

デモの構成:

- 互いに直接接続された3つのHydraノードからなるクラスタで、それぞれが3つのHydraクレデンシャル `alice`, `bob`, `carol` のいずれかにアクセス
- ローカル開発ネットとして稼働する単一のブロック生成カルダノノード
- メトリクス収集用Prometheusサーバー
- 個々のHydraノードと対話するためのアドホック端末ユーザーインターフェイスクライアント

```mdx-code-block
import DocCardList from '@theme/DocCardList';
import {useCurrentSidebarCategory} from '@docusaurus/theme-common';

<DocCardList items={useCurrentSidebarCategory().items}/>
```

<iframe style={{width: '100%', height: '480px'}} src="https://www.youtube.com/embed/dJk5_kB3BM4" title="Hydra Head Demo" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen="true"></iframe>

<br/><br/>

:::caution Disclaimer
このビデオは、例として基本的なターミナルユーザーインターフェイスを実演しています。裏側では、端末クライアントはHydraノードによって提供されるWebSocket APIに依存しており、これはアプリケーションが使用する可能性が高いものです。
:::
