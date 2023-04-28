---
sidebar_label: 'トポロジー'
sidebar_position: 1
---

# トポロジー

Hydra Headは、[コアコンセプト](/core-concepts) ページで説明されているように、よく定義されたレイヤ2コンセンサスプロトコルです。しかしこれは物語の終わりではなく、このプロトコルをどう使うか、どのようなトポロジーが可能かを実際に定義していません。トポロジーとは、Hydraノード、例えばプロトコルを実装した実際のソフトウェアが、どのように相互接続できるかを意味しています。

より多くのユーザがHydraの上にソリューションを実装するにつれて、このトポロジーの「カタログ」は、新規参入者が正しい展開モデル、例えば彼らのユースケースに最も適したものを見つけて構築するのに役立つよう拡張されていくでしょう。


```mdx-code-block
import DocCardList from '@theme/DocCardList';
import {useDocsSidebar} from '@docusaurus/theme-common/internal';

<DocCardList items={useDocsSidebar().filter(({ docId }) => docId != "index")}/>
```
