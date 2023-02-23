This document contains guidelines for anyone who decides to perform an assessment on the security of the Hydra Head protocol specification and implementation released from this repository.

Hydra is an open-source project and, as such, can be freely used, reviewd and audited. Shall you want to perform a
security audit, the hydra team would suggest to apply static and dynamic analysis, focused, but not limited to, the areas of concern below.

Shall you decide to share your findings with us, please consider the following, as Per [CIP-52](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0052):
1. Any discrepancies, deviations or spotted vulnerabilities shall be described and classified with an appropriate severity level. Recommendations to rectify the identified deficiencies shall also be provided whenever appropriate.
2. When automated tools are used as a replacement for manual review/code inspection, they shall be documented or referenced. Note that it’s the responsibility of the auditor to ensure that such tooling may not exhibit potential failures that can adversely affect the review outcome.
3. Any strategies/methodologies used to assess the consistency, correctness and completeness of the requirements shall also be documented or referenced.

# Context and assumptions

The Hydra Head protocol is implemented in the `hydra-node`, which connects to the Cardano network as layer 1 (L1) through a `cardano-node`, to other Hydra Head compliant nodes over an off-chain network, and exposes an API to users of the layer 2. Most relevant artifacts for an audit are:
 - Coordinated Hydra Head V1 Specification
 - Hydra plutus scripts (on-chain code)
 - Hydra node chain layer code (off-chain code)

As described in the following figure, the main entry points of a Hydra node are:
* The API through which a client can connect to the node;
* The network communication with other Hydra node peers;
* The transactions posted on the Cardano ledger (accessed through a network connection).
 
![artifacts.png](artifacts.png)

A detailed description of each of the artifacts relevant for an audit can be found in the above section _Artifacts_.

For its operations, the hydra-node process relies on a cardano-node process and client processes can connect to the hydra-node process through API.
Generally, we recommand to run the Hydra and cardano node in a trusted environment so that any assessment performed during an audit should be done under the following assumptions:
* Organizational procedures for managing the hydra and cardano signing keys are trusted;
* The system running the hydra-node is trusted;
* The cardano-node software is trusted as is the system running it;
* Any client software connecting through the API is trusted as are the systems running them;
* The communications between the cardano-node and the hydra-node are trusted;
* The communications between the hydra-node and any client software connecting through the API are trusted;
* The cardano-node is assumed to have a responsive communication line with the Cardano network such that the hydra-node can react to on-chain transactions in a timely manner. In particular, hydra-node is able to observe head closure and contest in a delay shorter than the contestation period fixed by the protocol.

What is not explicitly trusted in the above list is deemed untrusted. In particular, the other hydra nodes participating in a head are not trusted.

We would appreciate comments about any of the above. Especially if you can suggest less restricting assumptions under which the security properties of Hydra still hold. Or practical suggestions on how to implement these assumptions. We also appreciate recommendations about how to ensure these assumptions hold in a real environment.

If you plan to consider a different operational environment were these assumptions would not be true and plan to share your discoveries with us, please be as explicit as possible about the corresponding assumptions.

# Suggested Tasks

Broadly speaking, an audit would probably want to ensure that the security properties proven in _Coordinated Hydra Head V1 Specification_ hold for the implementation, taking also into acount the main entry points of a `hydra-node` which are the node to node network communications, the API and the Cardano ledger.
Furthermore, we suggest to focus efforts on ensuring correctness and robustness of the _Hydra plutus scripts_ (on-chain code) as it is harder (or impossible) to fix in the field and attackers could side-step all measures but the on-chain code (i.e. use their own off-chain code).

We suggest auditors to assess the following statements, which will be detailed in the next sections:
1. Coordinated Hydra Head V1 specification proofs are sound.
2. Hydra plutus scripts (on-chain code) are consistent with Hydra Head V1 specification and immune to common Cardano smart-contract weaknesses.
3. Hydra node chain layer code generates transactions which are consistent with Hydra Head V1 specification.

## Coordinated Hydra Head V1 specification proofs are sound

The Hydra Head V1 specification describes Coordinated Hydra Head V1 Protocol.

This specification provides several important security properties:
* Consistency: No two uncorrupted parties see conflicting transactions confirmed.
* Liveness: If all parties remain uncorrupted and the adversary delivers all messages, then every transaction becomes confirmed at some point.
* Soundness: The final UTxO set accepted on the mainchain results from a set of seen transactions.
* Completeness: All transactions observed as confirmed by an honest party at the end of the protocol are considered on the mainchain.

You could review this specification to share comments and assess that the above properties hold. An outcome of such a review could include, without being limited to:
* Identification of any inconsistencies or lack of generality within the specification;
* Identification of any inconsistencies in the proofs exposed in the specification;
* Identification of any behavior that could lead, with an adverserial mindset, to one of the above properties to be falsified.

## Hydra plutus scripts are consistent with Hydra Head V1 specification and immune to common Cardano smart-contract weaknesses

The Coordinated Hydra Head V1 specification defines the checks the on-chain scripts must perform for a functioning Hydra Head.

You could assess that the Hydra plutus scripts are consistent with the Coordinated Hydra Head V1 specification and immune to common possible vulnerabilities for Cardano smart contracts, such as but not limited to the following attacks:
* Execution cost limits;
* Double satisfaction;
* Replay;
* Denial of service.

The outcome of the review could also include, without being limited to:
* a validation that the Hydra plutus scripts validators do check the transaction constraints defined in the Coordinated Hydra Head V1 specification;
* a review and comment on the mutation-based tests applied to the Hydra plutus scripts and, in particular, any adversarial situation that would not be covered by them but should be.

See the documentation of our [Mutation-Based tests](https://hydra.family/head-protocol/haddock/hydra-node/tests/Hydra-Chain-Direct-Contract-Mutation.html).

See [Common Weaknesses](https://plutus.readthedocs.io/en/latest/reference/writing-scripts/common-weaknesses/index.html) and [Vulnerabilities](https://github.com/Plutonomicon/plutonomicon/blob/main/vulnerabilities.md).

## Hydra node chain layer code generates transactions which are consistent with Hydra Head V1 specification

The Coordinated Hydra Head V1 specification defines the transactions the off-chain code should build and post to evolve the head status on-chain.

You could assess that the Hydra node chain layer code can only build transactions which are consistent with the Coordinated Hydra Head V1 specification.

# Out of Scope

With version 0.9, the Hydra team suggests to focus on the scope described in the above sections and consider anything else as out of scope.
In particular, the following items shoudl be seen as out of scope:

* Hydra Head protocol implementation is immune to attacks via chain transactions.
* Hydra Head protocol implementation is immune to attacks via network.
* Hydra Head protocol implementation faithfully reflects the head state through the API.
* Hydra Head protocol implementation is immune to API attacks.
* Any attack which would be invalid under the assumptions stated above in the document.

# Artifacts

This sections gives a detailed description of the artifacts mentioned above in the document:
 - Coordinated Hydra Head V1 Specification
 - Hydra plutus scripts (on-chain code)
 - Hydra node chain layer code (off-chain code)

## Artifact 1: Coordinated Hydra Head V1 Specification

The Hydra Head protocol implementation derives from [Hydra: Fast Isomorphic State Channels](https://eprint.iacr.org/2020/299.pdf) in several ways. Especially some simplifications have been introduced and generalizations removed.

The [Coordinated Hydra Head V1 specification](https://www.overleaf.com/read/bbqzmptcxryj) captures these deviations and also includes the "formal notation" of the actual transaction constraints (which are foregone in the original paper). Also, it details the L2 protocol logic for the **Coordinated** Head protocol - which is implemented in V1.

## Artifact 2: Hydra Head Protocol Implementation

With Hydra Head Protocol Implementation we refer to the software component that is used to operate a node in the Hydra Head protocol. The `hydra-node` allows its users to open a head, lock funds in it, connect to peers, process transactions as a layer 2, close a head and unlock the corresponding funds. It is comprised by the Hydra plutus scripts, Hydra head chain layer, layer 2 code, network communication between peers, and an API for clients to connect and use the node.

* Source code repository: [input-output-hk/hydra](https://github.com/input-output-hk/hydra)
* Version ready to be audited: [0.9.0](https://github.com/input-output-hk/hydra/releases/tag/0.9.0)

The following parts are described below:
 - Hydra plutus scripts (on-chain code)
 - Hydra node chain layer code (off-chain code)

### Artifact 2.1: Hydra plutus scripts (on-chain code)

Hydra plutus scripts source code can be found in the [hydra-plutus module](https://github.com/input-output-hk/hydra/tree/master/hydra-plutus).

Hydra plutus mutation based testing can be found in the [hydra-node module](https://github.com/input-output-hk/hydra/tree/master/hydra-node) in test/Hydra/Chain/Direct/Contract/.

### Artifact 2.2: Hydra node chain layer code (off-chain code)

Hydra node chain layer code can be found in the [hydra-node module](https://github.com/input-output-hk/hydra/tree/master/hydra-node) in src/Hydra/Chain/Direct/

Hydra node chain layer tests can be found in the [hydra-node module](https://github.com/input-output-hk/hydra/tree/master/hydra-node) in test/Hydra/Chain/Direct/