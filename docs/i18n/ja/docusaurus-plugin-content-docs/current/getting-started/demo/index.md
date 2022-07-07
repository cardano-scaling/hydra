---
sidebar_position: 1
---

# Demo

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
