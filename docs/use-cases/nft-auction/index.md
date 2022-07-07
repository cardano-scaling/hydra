# NFT Auction

> Layer 1 redeemable vouchers negotiated on layer 2.

NFT drops can be a real problem when run on L1, generating a lot of UTxO and transactions in a short amount of time, clogging the network and leading to congestion and general slowdowns. The problem seems to come not much from the NFTs themselves but from the large of number of bidding transactions people post to grab some.

We sketch a way to run NFT auctions inside Hydra Head that would alleviate that problem. Here is an outline of the sequence of transactions involved, both on-chain and off-chain:

![](./diagram.png)

- The auctioneer forges some "auction tokens" (a.k.a VT) representing participation in an auction for some NFT;
- The auctioneer commits the ATs in a Head;
- The Auction ATs can be "grabbed" by bidders to bid some amounts for a given NFT auction;
- The bidders do not have to be Head parties<sup>1</sup>, they just do "normal" cardano transactions to grab a bid token and then possibly keep bidding until the auction ends;
- The auctioneer posts a settlement transaction that consumes all current bids at some point, producing a voucher for the NFT to be sent to Bob if he pays V2'. The voucher is only valid if produced by this script, and there might be some reserve price to ensure price does not fall below some threshold;
- Then Head is closed and the voucher is posted on-chain;
- Bob can redeem his NFT, paying V2' to the auctioneer.

The auctioneer runs the risk of opening/closing a Head with the winner not reclaiming his NFT. If it does not run the head itself, it runs the risk of the Head parties rigging the auction but in the worst case, the Head is closed with someone having a voucher to claim the NFT at a reserved price, or the NFTs themselves are paid back to auctioneer.

The bidders run the same risk of bidding in a rigged auction but in the worst case, they can refuse to pay for the NFT. Anyhow, this setup would offer already a much better security story than all the fully custodial NFT drops done on Cardano today

:::info NOTES 

1. In this scenario, the Head is initialised by the auctioneer. It could be a "Managed Head" or "Head-as-a-Service", e.g. The auctioneer does not run a Hydra node but uses some third-party provider to run the node. A single-party head might not seem to make much sense but in this case it's just a way to do Cardano transactions with smart-contracts faster than on the mainchain.

2. This use case is _extracted_ from a [conversation that happened on Github](https://github.com/input-output-hk/hydra-poc/discussions/116). Have a look at the conversation for details and/or to comment.
:::
