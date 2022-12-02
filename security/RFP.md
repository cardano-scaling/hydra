FIXME: Improve language uniformity and replace
* on-chain code -> Hydra plutus scripts
* off-chain code -> Hydra node chain layer
* HeadLogic code -> Hydra node logic layer

# 2 - Background

Hydra is the layer 2 (L2) scalability solution for Cardano, which aims to minimize transaction cost and increase transaction speed via low latency & high throughput.
Hydra Head is the first of whole family of protocols and embodies the foundation for more advanced constructions relying on isomorphic, multi-party state-channels. Detailed information can be found at https://hydra.family/head-protocol/.

# 4 - Project Scope

We are issuing this solicitation to perform an assessment of the security of the Hydra Head protocol implementation. The hydra team is looking for a comprehensive and best practice security audit to include, but not limited to, the areas of concern below. Any additional materials and documentation can be referenced and attached to your submission.

Per [CIP-52](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0052),
1. Any discrepancies, deviations or spotted vulnerabilities shall be described and classified with an appropriate severity level. Recommendations to rectify the identified deficiencies shall also be provided whenever appropriate.
2. When automated tools are used as a replacement for manual review/code inspection, they shall be documented or referenced. Note that it’s the responsibility of the auditor to ensure that such tooling may not exhibit potential failures that can adversely affect the review outcome.
3. Any strategies/methodologies used to assess the consistency, correctness and completeness of the requirements shall also be documented or referenced.

## Context and assumptions

The Hydra Head protocol is implemented in the `hydra-node`, which connects to the Cardano network as layer 1 (L1) through a `cardano-node`, to other Hydra Head compliant nodes over an off-chain network, and exposes an API to users of the layer 2. Most relevant artifacts for this audit are:
 - Coordinated Hydra Head V1 Specification
 - Hydra plutus scripts (on-chain code)
 - Hydra node chain layer code (off-chain code)
 - Hydra node layer 2 code (L2 code)

As described in the following figure, the main entry points of a Hydra node are:
* The API through wich a client can connect to the node;
* The network communication with other Hydra node peers;
* The transactions posted on the Cardano ledger (accessed through a network connection).
 
![artifacts.png](artifacts.png)

A detailed description of each of these artifacts can be found in the above section _Artifacts_.

For its operations, the hydra-node process relies on a cardano-node process and client processes can connect to the hydra-node process through API. Any assessment performed during this audit must be done under the following assumptions about the environment of the Hydra node implementation:
* The hydra and cardano signing keys storage and management are trusted;
* The hydra-node software is trusted as is the system running it;
* The cardano-node software is trusted as is the system running it;
* Any client software connecting through the API is trusted as are the systems running them;
* The communications between the cardano-node and the hydra-node are trusted;
* The communications between the hydra-node and any client software connecting through the API are trusted;
* The cardano-node is assumed to have a responsive communication line with the Cardano network such that the hydra-node can react to on-chain transactions in a timely manner. In particular, hydra-node is able to observe head closure and contest in a delay shorter than the contestation period fixed by the protocol.

What is not explicitly trusted in the above list is deemed untrusted. In particular, the other hydra nodes participating in a head are not trusted.

Any discovery not compliant with one of these assumptions is out of scope.

Responder can formulate comments about any of the above. Especially if they can suggest less restricting assumptions under which the security properties of Hydra still hold. Or if they would suggest practical suggestions on how to implement these assumptions. We accept recommendations about how to ensure these assumptions hold in a real environment.

## Tasks

Broadly speaking, our goal is to ensure that the security properties proven in the original publication hold for the implementation, taking also into consideration the main entry points of a `hydra-node` which are the node to node network communications, the API and the Cardano ledger. Furthermore, we want to focus efforts on ensuring correctness and robustness of the code running on-chain (L1) as it is harder (or impossible) to fix in the field and attackers could side-step all measures but the on-chain code (i.e. use their own off-chain code).

TODO: say something about increasing confidence to our users in using the hydra-node and the protocol implemented by it;

We are requesting proposals to assess the following statements, which will be detailed in the next sections:

-- Specification

1. Coordinated Hydra Head V1 specification is consistent with the original publication

-- Plutus scripts

2. Hydra plutus scripts are consistent with Hydra Head V1 specification
3. Hydra plutus scripts are immune to common Cardano smart-contract weaknesses

-- Implementation correctness

4. Hydra node chain layer code generates transactions which are consistent with Hydra Head V1 specification
5. Hydra node logic layer code is consistent with Hydra Head V1 specification

-- Implementation robustness

6. Hydra Head protocol implementation is immune to attacks via chain transactions
7. Hydra Head protocol implementation is immune to attacks via network
8. Hydra Head protocol implementation faithfully reflects the head state through the API

Out of scope:
0. Hydra Head protocol implementation is immune to API attacks -- out of scope because trusted

We will also accept proposals covering subsets of this list, but would like to get at least the highest priority tasks covered. The responder should briefly explain how they intend to address each task. If not all tasks are covered, individual estimation of efforts is required.

### Coordinated Hydra Head V1 specification is consistent with the original publication

The Hydra Head V1 specification describes Coordinated Hydra Head V1 Protocol.

This specification provides several important security properties (see Artifact 1 above).

Review this specification to give us comments and assess that the above properties hold.
The outcome of the review should include, but not being limited to:
* Identification of any inconsistencies or lack of generality within the specification;
* Identification of any inconsistencies in the proofs exposed in the specification;
* Identification of any behavior that could lead, with an adverserial mindset, to one of the above properties to be falsified.

TODO: should we ask, and how, the auditor to provide feedback about the Hydra Head V1 specification on clarity, ambiguity, readability and comprehensibility; it is fit to serve as a foundation for the implementation of the protocol?

FIXME: which property ensures that we don't lock funds in the head?

FIXME: v1 version of the specification with proof needed.

### Hydra plutus scripts are consistent with Hydra Head V1 specification

The Coordinated Hydra Head V1 specification defines the checks the on-chain scripts must perform for a functioning Hydra Head.

Assess that the Hydra plutus scripts are consistent with the Coordinated Hydra Head V1 specification.
The outcome of the review should include, bo not being limited to:
* a validation that the Hydra plutus scripts validators do check the transaction constraints defined in the Coordinated Hydra Head V1 specification;
* a review and comment on the mutation-based tests applied to the Hydra plutus scripts and, in particular, any adversarial situation that would not be covered by them but should be in the context of this audit;
* any comment about practical Cardano smart contract issue, absent form the specification, which would not be handled by the Hydra plutus scripts;

TODO: should we ask, and how, the auditor for feedbacks about potential scripts optimizations?

See the documentation of our [Mutation-Based tests](https://hydra.family/head-protocol/haddock/hydra-node/tests/Hydra-Chain-Direct-Contract-Mutation.html)

### Hydra plutus scripts are immune to common Cardano smart-contract weaknesses

Evaluate the Hydra plutus scripts susceptibility to common possible vulnerabilities for Cardano smart contracts, such as but not limited to the following attacks:
* Execution cost limits;
* Double satisfaction;
* Replay;
* Denial of service.

See [Common Weaknesses](https://plutus.readthedocs.io/en/latest/reference/writing-scripts/common-weaknesses/index.html) and [Vulnerabilities](https://github.com/Plutonomicon/plutonomicon/blob/main/vulnerabilities.md).

### Hydra node chain layer code generates transactions which are consistent with Hydra Head V1 specification

The Coordinated Hydra Head V1 specification defines the transactions the off-chain code should build and post to evolve the head status on-chain.

Assess that the Hydra node chain layer code can only build transactions which are consistent with the Coordinated Hydra Head V1 specification.

### Hydra node logic layer code is consistent with Hydra Head V1 specification

The Coordinated Hydra Head V1 specification defines not only the overall life-cycle of the Hydra Head, but also the layer 2 (L2) protocol logic. In the `hydra-node`, the `HeadLogic` layer implements this protocol logic.

Assess that the Hydra node logic layer code faithfully implements the off-chain logic described in the Coordinated Hydra Head V1 specification.
The outcome of the review should include, bo not being limited to:
* identify any discrepancy with the off-chain logic from the specification;
* for any such discrepancy, what security property could be impacted;
* a review of the tests applied to the Hydra node logic layer and, in particular, any situation that would not be covered by the tests.

This review should consider fair management of the input and output to and from the Hydra node logic layer.

See [Hydra.HeadLogic](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-HeadLogic.html)

### Hydra Head protocol implementation is immune to attacks via chain transactions

The `hydra-node` chain layer is in charge of observing transactions posted on the Cardano blockchain. _Relevant_ observations are then processed in the logic layer, to evolve the internal head status.

Review the Hydra node chain and logic layers in presence of invalid, unexpected or maliciously forged transactions.
The outcome of the review should include, but not being limited to:
* identification of transactions or a suite of transactions that would mislead a `hydra-node` into an incorrect head state
* identification of transactions or a suite of transactions that would lead to a denial of service for the `hydra-node`
* identification of transactions or a suite of transactions that would invalidate one of the security properties of the Coordinated Hydra Head V1 specification.

### Hydra Head protocol implementation is immune to attacks via network

The `hydra-node` network layer is in charge of sending/receiving messages to/from other Hydra nodes. _Relevant_ messages are then processed in the logic layer, to evolve the internal head status.

Review the Hydra node network and logic layers in presence of invalid, unexpected or maliciously forged network messages.
The outcome of the review should include, but not being limited to:
* identification of network attack that would mislead a hydra node to an incorrect head state
* identification of network attack that would lead to a denial of service for the hydra node
* identification of network attack that would invalidate one of the security properties of the Coordinated Hydra Head V1 specification.

Generic denial of service attack against the node itself is out of scope of this audit. One should only focus on specific hydra network layer attacks that could generate a Denial of Service because of an issue in the hydra-node itself.

### Hydra Head protocol implementation faithfully reflects the head state through the API

Given the assumptions above, the API is only accessible locally from the trusted machine running the `hydra-node`. Moreover, we don't consider the client API as an adversarial party so any attack through the API is considered out of scope. However, it is important that a running node does reflect the current stat of a head faithfully to its API client(s).

Review the Hydra node API layer and the corresponding artifacts.
The outcome of the review should include, but not being limited to:
* identification of network attack that would lead to an unfaithful representation of a hydra head through the API
* identification of transactions or a suite of transactions that would lead to an unfaithful representation of a hydra head through the API
* identification of a chain of events that would lead to an unfaithful representation of a hydra head through the API

## Out of Scope

The scope of this audit has been described in the above sections. What is not in scope is out of scope. In particular, the following items are out of scope of this audit:

- Verify the whole original paper and its proofs.
- Hydra Head protocol implementation is immune to API attacks -- out of scope because trusted
- Any attack which would be invalid under the above stated assumptions.

## Artifacts

This sections gives a detailed description of the aritfacts mentioned in this RFP:
 - Coordinated Hydra Head V1 Specification
 - Hydra plutus scripts (on-chain code)
 - Hydra node chain layer code (off-chain code)
 - Hydra node layer 2 code (L2 code)

### Artifact 1: Original publication

The Hydra Head protocol has first been published in [Hydra: Fast Isomorphic State Channels](https://eprint.iacr.org/2020/299.pdf). This paper describes several versions of the protocol (simple, with or without conflict resolution, incremental (de)commits, etc.), experimental validation, a security definition with corresponding security proofs of the following four properties:

* Consistency: No two uncorrupted parties see conflicting transactions confirmed.
* Liveness: If all parties remain uncorrupted and the adversary delivers all messages, then every transaction becomes confirmed at some point.
* Soundness: The final UTxO set accepted on the mainchain results from a set of seen transactions.
* Completeness: All transactions observed as confirmed by an honest party at the end of the protocol are considered on the mainchain.

A study of the whole paper and possible variations of the protocol is out of scope of this solicitation, but it serves as a good starting point and introduction to the overall protocol. Note that the implemented version of the Hydra Head protocol - named **Coordinated Hydra Head** - can be considered a subset of the "Simple Protocol without Conflict Resolution". That is, we recommend reading Chapters 2-6, where most of the on-chain "Protocol machine" is mostly consistent, but the off-chain "Head protocol" is different in the actual specification.

### Artifact 2: Coordinated Hydra Head V1 Specification

The Hydra Head protocol implementation derives from the original publication in several ways. Especially some simplification have been introduced and generalizations removed.

The [Coordinated Hydra Head V1 specification](https://docs.google.com/document/d/1XQ0C7Ko3Ifo5a4TOcW1fDT8gMYryB54PCEgOiFaAwGE/) captures these deviations and also includes the "formal notation" of the actual transaction constraints (which are foregone in the original paper). Also, it details the L2 protocol logic for the **Coordinated** Head protocol - which is implemented in V1.

Note that it is lacking some structure and introductory sections and we recommend to see Artifact 1 for that.

FIXME the following list is probably not useful since it should be in the spec
In particular, the following simplifications are done in the actual implementation:
* Kagg is a list of keys
* No hanging transactions, i.e. only snapshots are signed and can be used in close transaction
* ...

### Artifact 3: Hydra Head Protocol Implementation

With Hydra Head Protocol Implementation we refer to the software component that is used to operate a node in the Hydra Head protocol. The `hydra-node` allows its users to open a head, lock funds in it, connect to peers, process transactions as a layer 2, close a head and unlock the corresponding funds. It is comprised by the Hydra plutus scripts, Hydra head chain layer, layer 2 code, network communication between peers, and an API for clients to connect and use the node.

Source code repository: [input-output-hk/hydra](https://github.com/input-output-hk/hydra)
Version to be audited: [0.9.0](https://github.com/input-output-hk/hydra/releases/tag/0.9.0)

TODO describe the inputs and outputs of a hydra node

TODO: clarify which artifacts we need to introduce

TODO:We should share our test architecture/topology and also the test results here: https://hydra.family/head-protocol/benchmarks/tests/hydra-cluster/hspec-results

#### Artifact 3.1: Hydra plutus scripts

TODO

Grab stuff from https://hydra.family/head-protocol/haddock/hydra-plutus/index.html

#### Artifact 3.2: Hydra node chain layer code

TODO

Grab stuff from https://hydra.family/head-protocol/haddock/hydra-node/index.html sub-sections of _Hydra.Chain.Direct_

#### Artifact 3.3: Hydra node layer 2 code

TODO


