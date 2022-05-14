---
sidebar_position: 1
---

# Démo

```mdx-code-block
import DocCardList from '@theme/DocCardList';
import {useCurrentSidebarCategory} from '@docusaurus/theme-common';

<DocCardList items={useCurrentSidebarCategory().items}/>
```

<iframe style={{width: '100%', height: '480px'}} src="https://www.youtube.com/embed/dJk5_kB3BM4" title="Hydra Head Demo" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen="true"></iframe>

<br/><br/>

:::caution Avant-propos
Cette vidéo montre une interface utilisateur au sein d'un terminal. En coulisse, l'application cliente se connecte à un noeud Hydra local au moyen d'une WebSocket comme n'importe quelle application cliente souhaitant intéragir avec un noeud Hydra le ferait. Autrement dit, bien qu'il ne s'agisse pas d'un exemple des plus palpitants, c'est néanmoins un exemple possible et plausible d'application cliente.
:::
