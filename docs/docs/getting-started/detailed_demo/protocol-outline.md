---
sidebar_position: 3
---

# Protocol outline

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```
In this section, we will discuss a high-level overview of the different stages of the Hydra protocol and its life cycle. We assume that everything goes accordingly, by which we mean that during all steps  of the protocol, all parties are online and do not have a dispute. The cycle can be defined in the following four stages.

## Initializing

In this stage, the foundation of the protocol is laid. It all starts with a group of parties that together want to run an instance of a hydra head. A few things need to be determined before they can start a secure execution of the protocol.
		
Firstly, the parties need to communicate some basic things with each other. They each share the following things

- An IP address + port of their machine that will run the Hydra node.
- A Hydra verification key to identify them in the head.
- A Cardano verification key to identify them on the blockchain.
- Agree on the protocol parameters that they want to use in the Hydra head. 

The IP address and the port is needed so that other parties know how to establish a secure pairwise communication channel with each other. We leave out here what a secure connection entails. The two keys are needed to ensure that parties are cryptographically identified on the blockchain and in the Hydra head. And lastly, all participants need to reach an agreement on the used protocol parameters that will be used inside the head. More details will follow on all these four things.

Then, once each of the parties has the above information about the other parties, they each can start their Hydra node. This will establish a communication channel for the rest of the protocol execution.

Via this communication channel, one party can start the protocol by posting an **initialization** transaction on the blockchain. This transaction is made to a smart contract that keeps track of the identification keys describes above of the parties. This action is then observed by the other parties on the blockchain, they confirm this transaction and use this contract to join the protocol. They join by **committing** funds that they have to this contract. Here, the contract keeps track of what funds were put in by which party by linking the funds to their verification key. This in case that the protocol is aborted before the head is opened, so that each can reclaim their funds.

## Opening 

In this stage, the core of the protocol, which gives us the scalability properties, is run. After all parties have committed to the contract, any party can post a transaction on the blockchain to open the head. To do so they **collect** all the funds committed and combine them in the contract, the head is now open.

From this point, the committed funds by each party are represented in the hydra head as the initial snapshot. Remember that Hydra is an isomorphic state channel, this means it behaves and looks similar to the layer one blockchain. That is why these snapshots keep track of the state using the EUTxO model. More explicit, each snapshot consists of at least these things

- a number to indicate its order regarding other snapshots.
- a commitment to a collection of UTxO's that represent the state of the head.
- The signatures of all parties.

With each new transaction, the collection of UTxO's changes and a new snapshot is made. The time that it takes to perform this snapshot is dependent on the size of the UTxO collection, the number of parties in the head and their communication time. But, note that this time is certainly less than the 20 seconds per block. Also, note that the entire UTxO's collection is stored in the snapshot. Since the total size of the head might get gigantic as it is used, the commitment to a particular collection is stored instead. This is done via Merkle tree's, a computer science data structure that allows you to prove that a UTxO is part of the commitment without storing it in full.

## Closing

In this stage, the parties are done with their transactions in the head and want to close it. During the previous stage, they all gathered multiple ordered snapshots, each index by an increasing number. With these snapshots, any party can close the head at any time, they do this by making a transaction on the layer one blockchain that notifies the contract that they want to close the head. More detailed, they notify the contract of their last perceived known snapshot.
	
The other parties see this transaction happen on the blockchain and check with the snapshot number that this snapshot is also their last perceived snapshot. If not, they have some time to **contest** to that snapshot by providing a newer snapshot to the contract. The time they have is given as a parameter in the initialization phase.

Notice that no party can cheat and can publish an old snapshot, as any of the other parties can contest to that intermediate snapshot.

## Finalizing

In this stage, the head is closed, but the initial funds are still at the contract. To distribute these funds, the contract needs to **fanout** the collected UTxOs from the commitment phase given the latest snapshot. From the latest snapshot, the commitment to a collection of UTxO's can be extracted. Each party can use this Merkelised data structure to prove that an UTxO that they owned in the head is part of it of the commitment. The contract then allows parties to extract UTxO's from the contract to the associated address that corresponds to UTxO as in the Merkle Tree.

It is important to note here that value cannot be created at this fanout stage with respect to the commitment phase. Though native assets can be used and committed in a head (it's an isomorphic state channel), the creation of new ones in a head cannot be fanned out. This is because the mainchain has no scope on any transactions in a head, so in particular, the layer one is oblivious to any (in)correct execution of a minting policy in the head. So in conclusion, the value that enters a head in the commitment phase equals the value extracted in the fanout phase. As a concluding overview, the four stages above give the following diagram.

![hydra-head-lifecycle](./images/hydra-head-lifecycle.svg)