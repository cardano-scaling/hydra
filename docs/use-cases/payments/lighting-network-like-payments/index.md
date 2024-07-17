# Lightning network payments


We are in the process of restructuring the payments category to create a more user-focused roadmap for various application scenarios.

In this specific scenario, we plan to utilize the trust established within a large Staking Pool Operator (SPO) and its extensive user base. Our proposal involves the creation of a Multihead Hydra Network consisting of 6-8 nodes. This network will facilitate Lightning Network-style direct payment transactions, each secured by Hash Time Locked Contracts (HTLCs) to ensure successful fund refunds in the event of routing failures.

Furthermore, this cluster can connect to different Base Network Clusters on the edge nodes, creating a Directed Acyclic Graph (DAG). This structure will lay the groundwork for a Lightning-like network, aimed at enhancing efficiency beyond current payment channel solutions. Users staking with the involved SPO will gain access to direct payments through this foundational network.


Oracle nodes will play a crucial role in managing routing within this network. They will analyze the DAG and the pool of incoming payment requests (known as 'Invoices' in the Lightning Network context, though this terminology may change). These Oracle nodes will compete to identify the shortest paths within the DAG to process payments efficiently.

From a technical perspective, the base layer of this network will mirror that described in the [Delegated Head Network scenario](https://hydra.family/head-protocol/topologies/delegated-head/).

**Pros:**
- Faster transaction speeds.
- Lower transaction costs.
- Reduced load on Layer 1 blockchain.

**Cons:**
- Limited trust and consensus on Layer 1.
- Potential for transaction reversals if no viable routing path is found.



