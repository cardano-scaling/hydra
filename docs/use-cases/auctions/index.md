---
sidebar_label: Auctions
sidebar_position: 2
---

# Hydra for auctions

### NFT marketplaces on Cardano

Cardano simplifies the minting and transferring of non-fungible tokens (NFTs), as the accounting for non-ADA tokens is integrated directly into the ledger (alongside ADA) rather than relying on complex and potentially error-prone custom smart contracts. This streamlined approach has spurred a vibrant NFT ecosystem on Cardano, encompassing art, music, identity, real estate, gaming, service subscriptions, and more.

High-quality marketplaces have emerged on Cardano, offering platforms where users can list, view, and purchase a wide array of NFTs. These platforms feature user-friendly interfaces that neatly display images/animations, rarity charts, royalty terms, and other metadata. NFTs can be purchased at the seller’s listed price or through an alternative offer made by a buyer.

However, the novelty of the assets being tokenized on Cardano and the relatively small market size pose challenges for effective price discovery in the NFT sector.

### NFT auctions on Cardano and current constraints

Auctions are a proven mechanism for efficient price discovery, especially for novel or unique items such as artworks, complex allocations like radio spectrums, or situations prone to insider trading and collusion, such as bankruptcy fire sales. Unlike traditional marketplaces where a seller sets a price and the first buyer to match it secures the purchase, auctions allow the price to evolve through competitive bidding, culminating in the sale to the highest bidder.

Auctions need to be dynamic and efficient—bidders often enter auctions spontaneously with limited time and attention. A slow or cumbersome bidding process can deter participation. Implementing such an auction experience directly on Cardano’s main network (layer 1) poses challenges, as layer 1 transactions can take time to be added to a block (approximately 10–60 seconds) and to be confirmed with a low probability of rollback (several minutes to hours).

### Running auctions with Hydra

The bidding mechanism of auctions is ideally suited for scaling on layer 2 technologies such as Hydra, where transactions within a Hydra head are confirmed rapidly and attain immediate finality. An effective layer 2-powered auction service, leveraging Hydra’s capabilities, could significantly enhance the auction experience on Cardano.

We envision that a Hydra-based auction framework could become a standard component integrated into NFT marketplaces, games, and other web 3.0 applications. This integration would enable these platforms to incorporate digital asset auctions seamlessly. Additionally, it could stimulate a new ecosystem for professional scalability providers, offering layer 2 hosting services akin to the existing stakepool and emerging governance ecosystems.

### A way forward

The path to achieving this vision could unfold through a series of feasible milestones, leveraging Hydra and its future enhancements:

```mdx-code-block
import DocCardList from '@theme/DocCardList';

<DocCardList />
```

### Further reading

- Explore the foundational paper by IOG and MLabs on utilizing Hydra for auctions: [Implementing auction projects using Hydra](https://iohk.io/en/blog/posts/2023/01/20/implementing-auction-projects-using-hydra/)

- Check out the repository for a reference implementation of a delegated voucher auction using Hydra: [hydra-auction](https://github.com/mlabs-haskell/hydra-auction).
