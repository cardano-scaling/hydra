# Lighting Network Payments

> User wallet to user wallet payment network utilising trust in the large SPO.

:::caution This is a legacy article

The payments category will be restructured into a more consistent
use-case-centric roadmap of application scenarios.
:::

In this scenario we could utilize trust accumulated into large SPO and there broad base of users staking with them to form a Multihead Hydra Network upto 6-8 nodes, which will process Liging Network types of direct payment transaction, each with HTLC (Hash Time Locked Contract, ensuring sucessfull user fund refund in case of unseccseful routing).

A part of that, this cluster could be connected on each of edge nodes with different Base Network Cluster forming a DAG and therefore would form the fondation of the Ligthing like network, which would be much more efficient than exisiting Payment channels solutions. This fondation network would provide direct payments to all users staking with the involved SPO.

Routing in this network could be provided by Oracles nodes which could explore the formed DAG and pool of incoming payments requests (they are named Invoices in the context of Ligting Network, but this naming seems confusing) and compeete to find the shortest paths in the formed DAG to process the payments.

Technically the network base layer would look like as the network in the [Delegated Head Network] (https://hydra.family/head-protocol/topologies/delegated-head/] scenario)

Pros:
 - Speed of transactions
 - Cost of transactions
 - Not going to load L1

Cons:
 - You do not have trust and consensus of the L1
 - Transaction has a chance to be reverted (if there is no available route)
