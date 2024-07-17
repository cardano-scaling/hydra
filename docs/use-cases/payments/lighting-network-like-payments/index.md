# Lightning network payments


We are in the process of restructuring the payments category to create a more user-focused roadmap for various application scenarios.

In this specific scenario, we plan to utilize the trust established within a large stake pool operator (SPO) and its extensive user base. Our proposal involves the creation of a multi-head Hydra network consisting of six to eight nodes. This network will facilitate Lightning Network-style direct payment transactions, each secured by Hash Time Locked Contracts (HTLCs) to ensure successful fund refunds in the event of routing failures.

Furthermore, this cluster can connect to different base network clusters on the edge nodes, creating a Directed Acyclic Graph (DAG). This structure will lay the groundwork for a Lightning-like network, aimed at enhancing efficiency beyond current payment channel solutions. Users staking with the involved SPO will gain access to direct payments through this foundational network.

Oracle nodes will play a crucial role in managing routing within this network. They will analyze the DAG and the pool of incoming payment requests (known as 'invoices' in the Lightning Network context, though this terminology may change). These oracle nodes will compete to identify the shortest paths within the DAG to process payments efficiently.

From a technical perspective, this network's base layer will mirror that described in the [delegated head network scenario](https://hydra.family/head-protocol/topologies/delegated-head/).

**Pros:**
- Faster transaction speed
- Lower transaction costs
- Reduced load on the layer 1 blockchain.

**Cons:**
- Limited trust and consensus on layer 1
- Potential for transaction reversals if no viable routing path is found.



