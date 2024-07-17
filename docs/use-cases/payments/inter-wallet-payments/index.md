# Inter-wallet payments

Inter-wallet payments is a (semi-)custodial peer-to-peer (P2P) payment service through wallet providers.

:::caution This is a legacy article

The payments category will be restructured into a more consistent use-case-centric roadmap of application scenarios.
:::

A Hydra head has participant limitations, which makes developing a large-scale P2P payment solution complex when each peer is directly involved as a member of the head. While the long-term roadmap includes potential solutions like routing and networks of heads (referenced in [Interhead Hydra: Two Heads Are Better Than One](https://eprint.iacr.org/2021/1188) by Jourenko et al), intermediate solutions may involve trusted parties such as light wallet providers, supplemented by on-chain smart contracts to minimize trust requirements.

In this model, we envision a Hydra head formed by major light wallet providers within the network, all sharing a common interest in offering their users cost-effective and rapid inter-wallet transactions. Each wallet provider acts as a member of the head and processes transactions on behalf of their users. To engage in transactions within the head, users must initially lock the funds they intend to transfer on layer 2 within a specific contract. This contract allows any wallet provider to redeem funds from it, given they possess proof of payment issued by the user.

Wallet providers, through their interfaces, display the 'virtual balance' in each account, reflecting the activity on layer 2. The 'effective balance' — the actual value locked on layer one — remains unchanged. Users can request any wallet provider to unlock funds from the contract anytime, provided there are remaining funds. This process involves collecting the funds at one or more of these contracts, after which they are redistributed: some are returned to locked accounts, and some are returned to the user who requested the withdrawal.

This scenario assumes that traffic between wallet providers is somewhat symmetrical. This means users from one wallet typically spend and receive amounts similar to users from another wallet, maintaining balance within the system. Additionally, wallet providers have the authority to reject payments that would disrupt this balance.
