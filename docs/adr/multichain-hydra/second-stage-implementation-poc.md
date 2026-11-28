# Second-stage implementation PoC

**Build an Ethereum-backed Head on Anvil using three real Hydra processes.** Then add the ADA-for-native-ETH experiment, with its settlement guarantees chosen explicitly.

This follows the [first-stage refactor](first-stage-refactor.md). Contract experiments can start before all package moves finish. See the [roadmap](multichain-poc.md) for the short sequence.

## Devnet choice

| Network  | Local setup                                                                                | Assessment                                        |
| -------- | ------------------------------------------------------------------------------------------ | ------------------------------------------------- |
| Ethereum | Anvil process or container; funded accounts; manual mining, timestamps and snapshot/revert | Best first target; smoke-tested here              |
| CKB      | `offckb node`; funded accounts, miner and debugging proxy                                  | Easy setup; Head deadline design still unresolved |
| Ergo     | Private node, isolated configuration, initialized wallet and mined funding                 | Viable; more harness work                         |

These are development-loop judgments. Only Anvil was run for this research. [Anvil](https://www.getfoundry.sh/anvil/index.html), [OffCKB](https://github.com/ckb-devrel/offckb), [Ergo private chain](https://docs.ergoplatform.com/node/testnet/mine-your-own-chain/)

CKB's `since` lower bound does not supply the upper deadline needed for contests and deposit admission. An easy devnet does not resolve that protocol problem. [CKB specification](https://github.com/nervosnetwork/rfcs/blob/master/rfcs/0017-tx-valid-since/0017-tx-valid-since.md)

## Verified local setup

On 20 October 2026, the official Anvil 1.8.5 image passed these checks:

- Ten accounts started with 10,000 development ETH each.
- A transfer stayed pending until manually mined.
- The next block used the requested timestamp.
- Snapshot/revert removed the receipt and restored the balance.
- Replacement history produced a different hash at the same height.

The temporary container was removed; the image was cached. This proves devnet controls, not the proposed Head contracts or Ethereum adapter. The smoke transfer used an unlocked development account; acceptance tests must use signed transactions.

Start the tested configuration:

```bash
docker run --rm --name hydra-poc-anvil \
  --publish 127.0.0.1:18545:8545 \
  --entrypoint anvil \
  ghcr.io/foundry-rs/foundry@sha256:32c8ea9ef052a440cb1620175987a3f49eff8b068a0c6a3d09ebf7f5f9a0e043 \
  --host 0.0.0.0 --port 8545 \
  --chain-id 31337 --hardfork prague \
  --timestamp 1700000000 --gas-limit 30000000 --no-mining
```

In another terminal, check readiness:

```bash
curl --fail --silent --show-error \
  -H 'Content-Type: application/json' \
  --data '{"jsonrpc":"2.0","id":1,"method":"eth_chainId","params":[]}' \
  http://127.0.0.1:18545
```

Require result `"0x7a69"`. Before connecting Hydra, also verify genesis and deployment identity. Mine with the same RPC request using `"method":"evm_mine"`. Stop with `docker stop hydra-poc-anvil`.

Prague, the gas limit and the genesis timestamp are pinned **test settings**, not current-mainnet claims. Pin the compiler, EVM target, optimizer and libraries too. [Foundry release](https://github.com/foundry-rs/foundry/releases/tag/v1.8.5)

Anvil executes real EVM transactions but does not reproduce validator consensus, fork choice or public-network finality. Later qualify against an execution-plus-consensus devnet such as [Ethereum's Kurtosis package](https://github.com/ethpandaops/ethereum-package).

## Milestone A: Ethereum-backed Head

Success means three Hydra processes fund a Head, transact, withdraw, contest a stale close and settle native ETH. After certification, stopping two participants must leave the third able to defend and allocate payouts without fresh signatures.

### Initial profile

| Property           | Choice                                                                     |
| ------------------ | -------------------------------------------------------------------------- |
| Parties/assets     | Three fixed participants; native ETH only, in integer wei                  |
| Ledger             | UTxO transfer notes with ownership signatures, unique IDs and conservation |
| Certificates       | Unanimous secp256k1 ECDSA; versioned EIP-712 domain                        |
| Claims             | Indexed Merkle tree; consumed-claim bitmap; fixed final root               |
| Bounds             | At most 64 claims in either settlement candidate; initial batches of eight |
| Pending action     | One at a time under the funding-version rules                              |
| Timing             | 60 chain seconds for tests; separate boundary cases                        |
| Funding activation | Inclusion plus two descendant blocks, as a synthetic test policy           |
| Fees               | Separate L1 fee accounts and reserves                                      |
| Contracts          | Immutable factory/Head; no privileged escrow bypass                        |

These are proposed bounds to measure. The ledger must reject unauthorized or duplicate spends, invalid amounts, overflow, unsupported outputs and excessive claim sets on every transition. `Hydra.Ledger.Simple` is a test model, not this implementation.

Exclude arbitrary scripts, ERC-20s, dynamic membership and public-chain deployment. An Ethereum backend does not imply EVM execution inside the Head.

### Contract operations

| Operation           | Required behavior                                                       |
| ------------------- | ----------------------------------------------------------------------- |
| Create              | Fix Head identity, participants, keys and profile                       |
| Deposit             | Record exact native value, deposit ID, expiry and refund owner          |
| Increment / recover | Admit the deposit once, or refund it once                               |
| Decrement           | Remove authorized liabilities and allocate the exact withdrawal         |
| Close               | Select the commitment matching the on-chain funding version             |
| Contest             | Accept a newer compatible certificate within bounded extension rules    |
| Claim               | Consume unique proofs after the deadline; credit fixed beneficiaries    |
| Withdraw / finalize | Deliver credits safely; keep outstanding credits and refunds accessible |

Preserve [both pending-action commitments](multichain-plan.md#preserve-the-current-protocol). Bound both candidates, including pending deposits and withdrawals.

Bind signatures to network/genesis, deployment, Head, profile, funding version, snapshot number, both roots/counts and exact pending action. A stored snapshot certificate must serve both close and contest; no fresh signing round for each submission. [EIP-712](https://eips.ethereum.org/EIPS/eip-712)

Share encoding and rejection vectors across Haskell and Solidity. Include duplicate signers, integer bounds, altered beneficiaries and repeated output contents with different IDs. Measure code size and worst-case gas, including a single-claim exit. Reduce the supported bound if any valid claim cannot exit.

### Core and adapter work

- Extract foundational types below both `hydra-protocol` and `hydra-tx`; introduce independent chain/deposit references.
- Separate snapshot content from profile-specific signatures, commitments and proofs.
- Parameterize the existing state machine; preserve Cardano bytes and replay compatibility.
- Add the transfer ledger, profile-aware API/handshake and persisted network/deployment identity.
- Sign Ethereum transactions locally. Persist intent ID, payload, nonce, signed bytes and transaction hash before broadcast.
- Reconcile uncertain outcomes before retrying. Follow block hashes/parents and ordered authenticated logs; ignore reverted execution.
- Check persisted cursor hashes after reconnect, including same-height or lower-height replacements.

The [Chain contract](multichain-plan.md#required-adapter-behavior) defines the shared submission and observation rules. Deep rollback does not erase signatures: if activated funding disappears, stop unsafe signing and expose the failure.

### Proposed files

```text
hydra-protocol-types/             shared foundational definitions
hydra-protocol/                   parameterized state machine
hydra-ledger-transfer/            authorized ETH transfer ledger
hydra-chain-ethereum/
  src/Hydra/Chain/Ethereum.hs
  src/Hydra/Chain/Ethereum/
    Config.hs                    identity and profile checks
    RPC.hs                       native client
    Observer.hs                  canonical events and rollback
    Submission.hs                nonces and durable intents
    State.hs                     checkpoints
    API.hs                       native transaction services
    Profile.hs                   signatures and claims
    Contract.hs                  ABI and event decoding
contracts/ethereum/
  src/{HeadFactory,Head}.sol
  test/                          contract, fuzz and invariant tests
  vectors/                       cross-language evidence fixtures
  script/Deploy.s.sol
hydra-cluster/src/Hydra/Cluster/Ethereum.hs
demo/ethereum/                   pinned configuration and run manifest
```

Update the [cluster harness](../../../hydra-cluster), [justfile](../../../justfile) and Nix/CI lists. Keep Ethereum tools optional for ordinary Cardano development.

Proposed recipes, **not yet implemented**:

```text
just poc-ethereum        build, start, deploy, test and clean up
just poc-ethereum-demo   leave the environment running
just poc-ethereum-test   run failure/recovery cases
just poc-ethereum-stop   stop this run's resources
```

Use separate keys, fee accounts and databases for each process. Give each run its own directory, ports and container names. Record tool/image versions, compiler settings, code/configuration hashes, chain identity, deployments and seed. Retain logs, certificates, intent journals, receipts, traces and gas measurements on failure.

### Test control

Only the runner uses Anvil's development RPC. The adapter uses normal block, log, receipt and transaction APIs. Do not fabricate Head events or edit balances/storage to make tests pass.

Manual mining requires asynchronous submission: a command waiting for a receipt must not block the process responsible for mining. Use exact timestamps, snapshot/revert and readiness assertions. [Mining](https://getfoundry.sh/anvil/mining), [Custom RPC methods](https://getfoundry.sh/anvil/custom-methods)

Test the proposed boundaries explicitly:

```text
deposit admission < expiry; recovery >= expiry
contest <= deadline; payout > deadline
```

Read extended deadlines again. Hold transactions pending past expiry to verify execution-time checks.

For rollback tests, snapshot before an action, let Hydra observe it, then revert only Anvil and mine a different branch. Keep Hydra's processes and databases intact. Verify recovery by comparing canonical hashes, not only WebSocket notifications.

### Acceptance scenario

1. Create the empty Head. Deposit and admit 10 ETH per participant: 30 total.
2. Alice sends Bob 3 ETH off chain: balances `7, 13, 10`.
3. Bob withdraws 2 ETH: Head balances `7, 11, 10`. Save `S_old`.
4. Carol sends Alice 1 ETH: balances `8, 11, 9`. Persist `S_new` and payout data.
5. Alice closes with `S_old`. Stop Alice and Carol's Hydra processes.
6. Bob contests with stored `S_new`, waits for the resulting deadline and allocates all claims.
7. Collect the credits; reject duplicate claims. Verify 28 ETH of final payouts plus the earlier 2 ETH withdrawal.
8. Restart a participant and require convergence without duplicated effects.

Beneficiary wallets may collect allocated credits after their Hydra processes stop. Allocation and delivery are separate milestones. Add a pending-deposit case that refunds successfully after Head finalization.

| Failure group  | Required cases                                                                                       |
| -------------- | ---------------------------------------------------------------------------------------------------- |
| Evidence       | Wrong chain/Head/deployment/version, missing or duplicate signers, changed roots or beneficiaries    |
| Funding        | Increment/recovery race; close around increment/decrement; late admission                            |
| Disputes       | Exact deadline boundaries, repeated contests, bounded extensions                                     |
| Payout         | Empty state; 1/8/9/64 claims; duplicates; concurrent batches; failing receiver                       |
| Recovery       | Crash around broadcast/inclusion/cursor persistence; reconnect; duplicate logs; uncertain submission |
| Reorganization | Remove funding, close, contest and payouts; replace a block at the same height                       |

Compare decoded execution with a model of phase, version and liabilities. Account for active claims, pending deposits, uncollected credits and delivered value; keep fees and unsolicited surplus separate.

**Gates:** contract feasibility, shared vectors/Cardano compatibility, three-process lifecycle, then adversarial recovery. Revise the design if an allowed output cannot exit, funding races duplicate value, or integration requires a second Head state machine.

## Milestone B: cross-network payment

**Goal:** Alice pays 100 ADA, a liquidity provider receives the ADA, and Bob receives 0.01 native ETH. These are arbitrary demo amounts. The provider supplies ETH; no asset is wrapped or converted by the protocol.

Combine the existing [Cardano devnet harness](../../../hydra-cluster/src/CardanoNode.hs) with Anvil. A shared Head uses one participant set, transaction stream and global snapshot sequence, plus two native escrows.

Choose the settlement profile before implementing its recovery tests:

| Profile                                  | What it demonstrates                                       | Limitation                                            |
| ---------------------------------------- | ---------------------------------------------------------- | ----------------------------------------------------- |
| Cooperative final certificate            | Shared state and both payouts without an external attester | A missing final signature can lock funds indefinitely |
| Canonical Cardano result + test attester | Shared state; unilateral Cardano dispute; remote payout    | Attester can lie or stop Ethereum payout/refund       |
| Verified Cardano receipt                 | Canonical selection without our attester key               | Proof/verifier feasibility remains open               |
| HTLC payment                             | ADA/ETH exchange with specified timeout recovery           | Different topology; not one shared Head               |

The [trust notes](trust-notes.md) recommend a cooperative shared-state experiment and a separate HTLC recovery experiment. Neither satisfies all the original canonical-settlement acceptance criteria.

### Original canonical-settlement design

For the relay-based profile:

1. Bind both deployments, participants and network identities to one Head. Use fixed initial funding; defer later deposits/withdrawals.
2. Observe both deposits under their stability policies, then unanimously authorize activation on Cardano.
3. Make activation and funding abort mutually exclusive on Cardano. Ethereum refunds only against the authenticated abort; it has no independent timeout refund.
4. Certify one authorized swap assigning ADA to the provider and ETH to Bob. Bind the quote, recipients, funding epoch and both payout roots/counts.
5. Let Cardano close/contest select the global result. Ethereum accepts that exact result with the participant certificate and the pinned attester's statement.
6. Deliver both native payouts; allow a gas-funded submitter to pay Bob only at his committed address.

This needs new Cardano contracts and shared certificate encoding. Existing Head signatures do not become cross-chain evidence automatically. Verify the same bytes and signature rules in both VMs. [Cardano secp256k1 primitives](https://cips.cardano.org/cip/CIP-49)

Stop two participants after certification and require the remaining participant to defend a stale Cardano close. Then test incomplete funding, abort, relay outage/restart, replay and rollback. A dishonest attester's false outcome is a known limitation, not something the test verifier can detect.

A later proof must authenticate a stable, irreversible Cardano settlement receipt. Inclusion of a contestable close transaction is insufficient.

Add a separate settlement coordinator above the chain adapters. Track `Certified`, result selection, each native delivery and `Complete` separately. One payout is not completion, and a later source-chain rollback cannot undo ETH already delivered.
