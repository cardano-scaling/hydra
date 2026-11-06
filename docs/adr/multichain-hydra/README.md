# Multichain Hydra

Design notes, not accepted decisions or implemented features.

| Note                                                     | Purpose                                                  |
| -------------------------------------------------------- | -------------------------------------------------------- |
| [PoC roadmap](multichain-poc.md)                         | Short overview and decision points                       |
| [Architecture](multichain-plan.md)                       | Candidate networks, contracts and the `Chain` interface  |
| [First-stage refactor](first-stage-refactor.md)          | File moves and behavior-preserving cleanup               |
| [Implementation PoC](second-stage-implementation-poc.md) | Devnet setup, implementation and acceptance tests        |
| [Trust notes](trust-notes.md)                            | Shared settlement, HTLCs and alternatives to an attester |

Research baseline: 20 October 2026, checkout `1b10c014295bf3e360431b4e0a24629388e50aa2`. Anvil startup, mining and history replacement were smoke-tested. No new Hydra backend or cross-chain contract has been implemented. The trust notes refine the original relay proposal; each settlement option has different recovery guarantees.
