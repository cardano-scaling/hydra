---
sidebar_label: 'Topologies'
sidebar_position: 1
---

# Topologies

Hydra Head is a well-defined Layer 2 consensus protocol as explained in the [Core Concepts](/core-concepts) page, but this is not the end of the story and does not really define _how to use_ this protocol and what _topologies_ are possible. While [example use cases](/use-cases) should help in understanding the former, the _topologies_ below explain some of the various ways in which Hydra Nodes and Hydra Heads could be deployed and interconnected.

As more users implement solutions on top of Hydra, this "catalog" of topologies shall expand to help newcomers find and build the deployment model that suits best their use case.


```mdx-code-block
import DocCardList from '@theme/DocCardList';
import {useDocsSidebar} from '@docusaurus/theme-common';

<DocCardList items={useDocsSidebar().filter(({ docId }) => docId != "index")}/>
```