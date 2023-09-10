# Lightning Network Payments

**Caution: This is a legacy article**

We are in the process of restructuring the payments category to create a more user-focused roadmap for various application scenarios.

In this particular scenario, we aim to leverage the trust built within a large Staking Pool Operator (SPO) and its extensive user base. We propose the formation of a Multihead Hydra Network consisting of 6-8 nodes. This network will facilitate Lightning Network-style direct payment transactions, each secured by Hash Time Locked Contracts (HTLCs) to ensure successful user fund refunds in case of routing failures.

Furthermore, this cluster can connect to different Base Network Clusters on the edge nodes, creating a Directed Acyclic Graph (DAG). This foundation will serve as the basis for a Lightning-like network, offering improved efficiency compared to existing payment channel solutions. Users staking with the involved SPO will have access to direct payments through this foundation network.

To manage routing within this network, Oracle nodes will play a crucial role. They will explore the formed DAG and the pool of incoming payment requests (referred to as "Invoices" in the context of the Lightning Network, although this naming may be confusing). These Oracle nodes will compete to find the shortest paths within the DAG to process the payments efficiently.

From a technical perspective, the network's base layer will resemble the network in the [Delegated Head Network scenario](https://hydra.family/head-protocol/topologies/delegated-head/).

**Pros:**
- Faster transaction speeds
- Lower transaction costs
- Reduced load on Layer 1 (L1) blockchain

**Cons:**
- Lack of trust and consensus on the L1
- Possibility of transaction reversal (if no available route is found)


