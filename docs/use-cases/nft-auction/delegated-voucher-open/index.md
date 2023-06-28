# Delegated Voucher Auctions (Open & Fully Collateralised)

> Possible extension of the initial prototype enabling open auctions, where bidders can freely participate without permission from sellers. It would require bids to be fully collateralized upon submission.


## Overview

The private invitational auction from the initial prototype is a very common use case, but there is also demand for a more casual auction format, whereby bidders can freely participate in any public auction without any vetting from sellers.
However, for sellers to be comfortable with this auction format, we need stronger backing for bids to mitigate the risk of bidders attempting to dishonor their bids – ideally, all bids should be fully collateralized by bidder deposits towards the auction. Thus, as a possible extension of the initial prototype auction design , we can replace the fixed security deposit from the prototype with a method that allows bidders to place sufficient deposits on L1 and then show proof of these deposits whenever they make bids on L2.
:::tip [Diagram of public auction workflow]
:::

## Workflow

In this step, each public auction would work as follows:
1. Delegates initialize a Hydra Head, making it available to host the bidding for a yet-to-be-announced auction.
2. The seller announces an auction to sell a given NFT (the “auction lot”), sets the terms of the auction, deposits the NFT into the auction smart contract, and indicates which Hydra Head will host the auction’s bidding process on L2.
3. Near the bidding start-time, the bidding process for the auction moves from L1 (where the auction was announced) to L2 (the delegates’ Hydra Head).
4. Bidders submit bids to delegates, who collectively witness the bids in the Hydra Head ledger and confirm bids to bidders via multi-signed ledger snapshots.
A bid is valid only if proof is attached of a sufficient deposit placed by the bidder on L1 towards the auction. Bidders make these deposits as needed on L1, to support their bids on L2.
5. At the end of the bidding phase, the Hydra Head is closed and the standing bid is moved to L1. The bidder that submitted that standing bid becomes the winner of the auction.
6. At the end of the bidding phase, the standing bid and the winning bidder’s deposit are used to send the auction lot to the winning bidder and the winning bid payment to the seller.
7. At the end of the bidding phase, all other bidders can reclaim their bid deposits.

## Prerequisites from Hydra technical roadmap
No specific prerequisites are currently anticipated, but some may come up.

## Remaining limitations

- Auction implementation is a prototype, not necessarily ready for production.
- A new Hydra Head must be opened for every auction.
- Delegates can censor bidders from submitting bids to the auction.
- Delegates are responsible on L2 for ensuring that the standing bid of the auction can only be replaced by a new bid that exceeds it by the minimum increment defined in the auction terms. However, bidders receive a multi-signed proof for every confirmed bid, which can be used as incontrovertible proof against delegates if there’s any foul play.
