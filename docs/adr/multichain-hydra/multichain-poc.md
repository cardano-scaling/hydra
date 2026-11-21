# Multichain PoC roadmap

**Goal:** prove an Ethereum backend, then demonstrate a Cardano wallet paying ADA for native ETH delivered to an Ethereum wallet.

The liquidity provider supplies ETH and receives ADA. A single shared Head remains the target for the shared-state experiment; HTLCs offer a separate payment route.

## A. Ethereum backend

- **Harness:** pin Anvil; automate readiness, mining, time and cleanup.
- **Contracts:** prove funding, certificates, disputes and bounded payouts.
- **Core:** extract the minimum chain/profile interfaces; preserve Cardano behavior.
- **Ledger and adapter:** add authorized ETH transfers, durable submission and canonical observation.
- **End to end:** run three Hydra processes; fund, transfer, close and verify native payouts.
- **Recovery:** stop two participants; contest stale state, refund pending funding and test restart/rollback.

Review contract feasibility before integration, then review reuse after the first complete lifecycle. See the [implementation plan](second-stage-implementation-poc.md).

## B. Cross-network experiment

- **Choose guarantees:** use the [trust notes](trust-notes.md) to select the settlement profile.
- **Run both chains:** Cardano devnet plus Anvil; Alice, Bob and a liquidity provider.
- **Fund:** lock 100 ADA and 0.01 ETH, with separate fee reserves.
- **Share state:** bind both escrows, the participant set and both allocations into one certified transaction.
- **Settle:** pay ADA to the provider and native ETH to Bob; assert actual wallet balances.
- **Exercise failure:** test missing funding, withheld signatures, interrupted settlement and the chosen profile's recovery path.

The simplest shared-state demo uses a unanimous **final certificate**, with possible lockup before it exists. The original Cardano-authoritative design instead uses a **trusted test attester**; removing that authority requires verified settlement evidence or further protocol work.

For a payment demo with timeout recovery, extend the existing HTLC work using a Cardano Head and Ethereum escrow. This is a useful separate experiment; it does not establish a single shared Head.

All recipes and contracts remain proposed. The [Anvil smoke test](second-stage-implementation-poc.md#verified-local-setup) only establishes local development tooling.
