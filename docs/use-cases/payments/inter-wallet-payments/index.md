# Inter-Wallet Payments

> A (semi-)custodial p2p payment service through wallet providers.

A Hydra head is limited in terms of the number of participants. Because of this, building a large-scale peer-to-peer payment solution can't be done naively where each peer is itself a member of the Head. While the long-term roadmap already envisions routing solutions and networks of heads (see also [Interhead Hydra: Two Heads Are Better Than One](https://eprint.iacr.org/2021/1188) by Jourenko & al), intermediate approaches may involve trusted parties such as light wallet providers. Plus, one can rely on additional on-chain smart contracts to reduce the trust assumption as much as possible.

In this scenario, we imagine a Hydra Head formed by a few major light wallet providers of the network who all have a common interest in providing their users with cheap and fast inter-wallet payments. Therefore, each wallet provider is a Head member and processes transactions inside the Head on behalf of their users. To transact inside the Head, users must first lock funds they want to move on layer 2 inside a particular contract. This contract authorizes any wallet provider to redeem funds from it, provided they have proof of payment issued by the corresponding user. 

Through their interface, wallet providers can show the _virtual balance_ held in each account based on the traffic on layer two. The _effective balance_ (i.e. the value locked on the layer one) remains unchanged. Users may ask any wallet provider to unlock funds in the contract at any time if any are left. This is done by collecting the funds at one or more of those contracts. Then funds are re-distributed accordingly: some back in locked accounts and some back to the user who requested his money back.

We assume in this scenario that the traffic between wallet providers is somewhat symmetrical (that is, users of wallet X spend/receive roughly as much as users of wallet Y) so that there isn't too much imbalance in the system. Furthermore, it can be enforced by wallet providers who can reject specific payments if their processing invalidates the assumption.
