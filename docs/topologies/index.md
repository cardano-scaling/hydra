---
sidebar_label: 'Topologies'
sidebar_position: 1
---

# Topologies

The Hydra Head protocol is a well-defined layer 2 consensus protocol, as detailed in the [developer documentation section](/docs/dev). However, understanding the protocol does not fully address how to implement it on a larger scale or explore the potential _topologies_ that can be achieved. While the [example use cases](/use-cases) help elucidate potential applications, the _topologies_ described below offer insights into various ways Hydra nodes and Hydra heads could be deployed and interconnected.

As the community grows and more users develop solutions on top of Hydra, the 'catalog' of topologies will expand. This expansion aims to assist newcomers in discovering and constructing the deployment model that best fits their use case.


```mdx-code-block
import DocCardList from '@theme/DocCardList';
import {useDocsSidebar} from '@docusaurus/theme-common/internal';

<DocCardList items={useDocsSidebar().items.filter(({ docId }) => docId != "index")}/>
```