---
sidebar_label: "Use cases"
sidebar_position: 1
---

# Hydra use cases

These articles explore payments, auctions, and other applications of Hydra.
Some describe prototypes or design proposals rather than supported products.
Check each integration's node version, trust assumptions, and recovery
behavior before adopting it. The [current limitations](/docs/known-issues)
apply to every application.

Contributions should explain the user workflow, the role of head operators,
and how funds return to L1 when a participant stops cooperating.

```mdx-code-block
import DocCardList from '@theme/DocCardList';
import { useDocsSidebar } from '@docusaurus/plugin-content-docs/client';

<DocCardList items={useDocsSidebar().items.filter(({ docId }) => docId != "index")}/>
```
