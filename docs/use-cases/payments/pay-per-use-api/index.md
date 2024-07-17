# Pay-per-use API

The pay-per-use API enables micro-payments between a service provider and a client.

:::caution This is a legacy article
The payments category will be restructured into a more consistent use-case-centric roadmap of application scenarios.
:::

Consider a web service for transaction datum resolution provided as an HTTP API. This service returns the pre-image of a given datum hash if it exists. At the same time, a light wallet provider, offering full Cardano smart contract support and prioritizing user experience and security, utilizes this service. The provider's workflow involves looking up datums and scripts included in transactions from various DApps its users interact with. By resolving these datums, the wallet can more accurately display the impact of specific transactions on contracts and their data.

To streamline their infrastructure, the light wallet implementors decide to utilize this external service for datum resolution. Given the novelty of their product, it’s challenging to precisely estimate the required resources and potential user base. Consequently, they enter into an agreement with the service provider to create a Hydra head for exchanging short-lived API credentials through micropayments.

Specifically, the service provider charges 10 lovelace per API lookup. In the deployed infrastructure, the light wallet backend server maintains a long-running connection with the service provider via a Hydra head. Each user request triggers a corresponding transaction within the head. In return, the API provider issues a one-time token credential that authorizes the API request. This interaction completes with sub-second delays without incurring any transaction fees.

![](./diagram.png)

Periodically, the light-wallet service replenishes its head with incremental commits (ie, a layer 1 transaction that locks additional value inside the head). Similarly, the service provider occasionally redeems its accumulated funds back to layer 1 through incremental decommits. These financial adjustments occur without needing to close the head.

:::tip Rewards
If mutually agreed upon, both parties can delegate the funds locked inside the head. This arrangement might allow, for instance, the light wallet to continue earning rewards on amounts locked in the head until the service provider redeems them. Essentially, participants can still generate passive income through network rewards to offset the operational costs of maintaining the head.
:::

Ultimately, the light wallet service pays only for what it uses, embodying the essence of cloud computing. This setup is also advantageous for the service provider because it allows them to share their Hydra infrastructure across multiple clients, thereby spreading costs and enhancing profit margins. Additionally, they benefit from direct ada earnings and avoid transaction fees on client payments.

It’s worth noting that numerous configurations are possible for this type of arrangement between a client and service provider. For example, API tokens could be treated as fungible tokens, like credits, purchased by the client and expended on each request within the head. In such a scenario, the server could:

- Limit its circulating supply to match its capacity
- Create a secondary marketplace for excess credits that clients have purchased but do not intend to use.
