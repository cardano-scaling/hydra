---
sidebar_label: Delegated voucher: open
sidebar_position: 2
---

# Delegated voucher auctions (open)

This is an extension of invitational auctions to enable open auctions, where bidders can freely participate without sellers' permission. Bids must be fully collateralized upon submission.

## Overview

The private invitational auction from the initial prototype is a very common use case. However, there is also demand for a more casual auction format where bidders can freely participate in any public auction without any vetting from sellers. For sellers to be comfortable with this auction format, we need stronger backing for bids to mitigate the risk of bidders attempting to dishonor their bids. Ideally, all bids should be fully collateralized by bidder deposits toward the auction. 

As a possible extension of the initial prototype auction design, we can replace the fixed security deposit from the prototype with a method that allows bidders to place sufficient deposits on layer 1 and then show proof of these deposits whenever they bid on layer 2.

## Workflow

The workflow for each public auction proceeds as follows:

1. **Hydra head initialization**. Delegates set up a Hydra head to host the bidding for an upcoming auction.
2. **Auction announcement**. The seller announces the auction, detailing the terms and depositing the NFT into the auction smart contract, and indicates which Hydra head will handle the bidding on layer 2.
3. **Transition to bidding**. As the bidding start-time approaches, the auction transitions from layer 1, where it was announced, to layer 2, managed by the delegate's Hydra head.
4. **Bid submission and validation**. Bidders submit their bids along with proof of a sufficient layer 1 deposit. Delegates verify the bids and confirm them to bidders via multi-signed ledger snapshots within the Hydra head.
5. **Closing the auction**. At the end of the bidding phase, the Hydra head is closed, and the highest bid is confirmed on layer 1. The bidder with the highest bid becomes the auction winner.
6. **Transaction completion**. The winning bidder’s deposit and the standing bid are used to transfer the auction lot to the winner and the payment to the seller.
7. **Deposit reclamation**. All other bidders reclaim their deposits after the bidding phase ends.

## Prerequisites from Hydra technical roadmap

Currently, no specific prerequisites are anticipated, though new requirements may emerge during development.

## Remaining limitations

- The auction system is still in the prototype stage and may not be ready for production.
- A new Hydra head is required for each auction.
- Delegates have the potential to censor bidders from submitting bids.
- Delegates are tasked with ensuring on layer 2 that any new bid exceeds the standing bid by at least the minimum increment specified in the auction terms. Bidders receive multi-signed proof for each confirmed bid, providing incontrovertible evidence against potential delegate misconduct.
