# Delegated Voucher Auctions (Invitational)

> The first complete prototype of an auction that can host its bidding process on L2.

:::tip This use case is under development

This project is an ongoing effort in collaboration with MLabs. Source code and documentation can be found [here](https://github.com/mlabs-haskell/hydra-auction).

:::

## Overview

As the first building block of what might grow, with the help of the Cardano community, into a Hydra-based auction framework, we develop a first complete prototype of an auction that can host its bidding process on a Cardano Hydra Head. Among the design candidates considered in our [Jan 2023 paper](https://iohk.io/en/blog/posts/2023/01/20/implementing-auction-projects-using-hydra/), the delegated voucher auction makes the best use of the respective strengths of Cardano mainnet (L1) and the Hydra Head protocol (L2) while mitigating their weaknesses.

![](./running-auctions-on-cardano.png)

Cardano ensures the integrity of its mainnet ledger by broadcasting each transaction across a large globally-distributed network of independent nodes and randomly selecting a slot leader among them to add the next block to the chain. This makes it exceedingly hard to add an invalid transaction to the ledger, censor a new transaction from reaching the ledger, or contradict the existence and position of a stable transaction within the ledger. Furthermore, Cardano’s Ouroboros consensus protocol is robust to nodes freely entering and leaving the network, as long as an honest majority of stake is maintained. However, Cardano mainnet has a high overhead of cost and latency, because every transaction and block must be broadcast, stored, and validated on every node across the world.
The Hydra Head protocol massively reduces this cost and latency overhead by distributing its ledger across a much smaller network of direct participants in the protocol and only tracking transactions that interact with a small subset of the mainnet UTxOs. However, it is up to the direct participants in the Hydra Head protocol to maintain the integrity of the ledger and all participants must unanimously agree on every transaction that gets added to the ledger. This means that anyone committing funds to a Hydra Head must either directly participate in the protocol or else effectively grant custody over the committed funds to the direct participants. It also means that no one can do anything in the Hydra Head without unanimous consent from the direct participants. Furthermore, the Hydra Head protocol requires all participants to be online and responsive to each other, or else it will stall its progress.

## Design

[Summary table of the features of the design]
We want the bidding experience to be lively and efficient, which is difficult to provide on L1, so we host the bidding process of our auction on L2. However, we can’t have the bidders be direct participants in the Hydra Head, because it is impossible to get their unanimous consent on every bid since they are competing against each other in the auction.
Instead, we define an independent group of delegates who will directly participate in the Hydra Head protocol on the seller's and bidders’ behalf to witness the bids. These delegates perform a role similar to the combination of stakepool and lightwallet providers on Cardano L1.
Since the bidders and seller are not direct participants in the Hydra Head, we do not commit any of the bidders’ funds or the seller’s NFT auction lot to the auction, because we do not want to grant the delegates custody over them. Instead, the Hydra Head is used purely to manage the informational aspect of the bidding process (Which bids were made? What is the current standing bid?). The auction lot and the bidders’ funds remain on L1 at all times.
Since the bidders’ funds are not present on the Hydra Head ledger, there is no way to provide proof to validators on L2 that any bidder has sufficient funds committed to honoring his bid. Instead, we allow the seller to require that bidders place fixed security deposits into a smart contract on L1 towards the auction, such that the seller can claim the winning bidder’s security deposit if the winning bid is dishonored, as compensation for wasting everyone’s time.]
The fixed deposit provides some protection to the seller against dishonorable behavior from bidders, but in some cases (e.g. if bids get unexpectedly high), it may still be advantageous to a bidder to dishonor his genuinely-placed bid or even to place a disingenuous bid to sabotage the auction. To feel comfortable selling an NFT in an auction with a fixed-size security deposit, the seller needs to be able to manage this risk. Furthermore, the seller may need to apply know-your-customer (KYC) and anti-money-laundering (AML) processes to the auction.
For these reasons, we implement an invitational private version of the auction where the seller has absolute discretion over which bidders may participate in the auction. The public version of the auction where bidders can freely participate will be implemented in a later milestone.

## Workflow

In this prototype, each auction would work as follows:
1. Delegates initialize a Hydra Head, making it available to host the bidding for a yet-to-be-announced auction.
2. The seller announces an auction to sell a given NFT (the “auction lot”), sets the terms of the auction (including the security deposit amount required), deposits the NFT into the auction smart contract, and indicates which Hydra Head will host the auction’s bidding process on L2.
3. Prospective bidders register their interest to participate in the auction by placing security deposits into the auction smart contract.
4. Near the bidding start time (defined in the auction terms), the bidding process for the auction moves from L1 (where the auction was announced) to L2 (the delegates’ Hydra Head).
5. The seller invites bidders to participate in the auction and starts the auction’s bidding phase. The seller has absolute discretion on which bidders to invite into the auction, based on the security deposits placed and KYC/AML considerations.
6. Bidders submit bids to delegates, who collectively witness the bids in the Hydra Head ledger and confirm bids to bidders via multi-signed ledger snapshots.
7. At the end of the bidding phase, the Hydra Head is closed and the standing bid is moved to L1. The bidder that submitted that standing bid becomes the winner of the auction.
8. Up to the voucher expiry time, the winning bidder can purchase the auction lot for the standing bid price.
9. After the voucher expiry time, if the winning bidder has not purchased the auction lot, the seller can reclaim it and claim the winning bidder’s security deposit for the auction. Otherwise, the winning bidder can reclaim his security deposit.
10. All other bidders can reclaim their security deposits at the end of the bidding phase.

## Prerequisites from Hydra technical roadmap

The prerequisites for Hydra for this milestone mainly deal with support for using smart contracts on L2:
During the design and early implementation phase of this milestone, we identified the following prerequisites to enable using the auction smart contracts on L2.
- Add a method to the Hydra node API to commit UTxOs from script addresses.
- Support committing multiple UTxOs per Hydra Head participant, so that collateral UTxOs can be committed to the Hydra Head for transactions with validator scripts on L2.
- Allow time to pass on the L2 ledger (instead of maintaining time fixed at the start time of the Hydra Head).
Fortunately, the Hydra core devs implemented all of these features during the course of the milestone. Thank you! 🚀

## Remaining limitations

This initial prototype auction design still has the following limitations:
- Auction implementation is a prototype, not necessarily ready for production.
- Open auctions (where bidders can freely enter any auction to bid) are not supported.
- Bids are backed only by fixed security deposits from bidders, which may be less than the full bid amount.
- A new Hydra Head must be opened for every auction.
- Delegates can censor bidders from submitting bids to the auction.
- Delegates are responsible on L2 for ensuring that the standing bid of the auction can only be replaced by a new bid that exceeds it by the minimum increment defined in the auction terms. However, bidders receive a multi-signed proof for every confirmed bid, which can be used as incontrovertible proof against delegates if there’s any foul play. In principle, this could allow for off-chain arbitration mechanisms to resolve disputes and/or an additional smart contract module where delegates could provide deposits that would be slashable if such evidence is provided.
