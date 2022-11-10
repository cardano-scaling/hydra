FIXME: Improve language uniformity and replace
* on-chain code -> Hydra plutus scripts
* off-chain code -> Hydra node chain layer
* HeadLogic code -> Hydra node logic layer

# 2 - Background

Founded in 2015 by Charles Hoskinson and Jeremy Wood, IOG is a technology company that builds blockchains and blockchain-based products for academic institutions, government entities, and corporations. We are a decentralized company that loves small, innovative teams forming and executing ideas that cause cascading disruption. Cascading disruption is the idea that most of the structures that form the world's financial, governance, and social systems are inherently unstable, and thus minor perturbations can cause a ripple effect that fundamentally reconfigures the entire system. Our company is committed to identifying and developing technology to force these perturbations in order to push towards a more fair and transparent order.


Hydra is the layer 2 scalability solution for Cardano, which aims to increase transaction speed through low latency and high throughput and minimize transaction cost.
IOG's product Hydra Head is the first protocol of the Hydra family and embodies the foundation for more advanced deployment scenarios relying on isomorphic, multi-party state-channels. Detailed information can be found at https://hydra.family/head-protocol/.

# 4 - Project Scope

IOG is issuing this solicitation to perform an assessment of the security of the Hydra Head protocol implementation. IOG is lloking for a comprehensive and best practice Secuirty Audit to include, but not limited to, the areas of concern below. Any additional materials and documentation can be referenced and attached to your submission.

Per [CIP-52](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0052),
1. Any discrepancies, deviations or spotted vulnerabilities shall be described and classified with an appropriate severity level. Recommendations to rectify the identified deficiencies shall also be provided whenever appropriate.
2. When automated tools are used as a replacement for manual review/code inspection, they shall be documented or referenced. Note that it’s the responsibility of the auditor to ensure that such tooling may not exhibit potential failures that can adversely affect the review outcome.
3. Any strategies/methodologies used to assess the consistency, correctness and completeness of the requirements shall also be documented or referenced.

## Context and artifacts

The Hydra head protocol implementation is composed of several parts, Hydra plutus scripts (on-chain code), Hydra node chain layer (off-chain code), Head logic (layer 2 code) and this implementation is based on general and specific specification which contain proofs of several properties of the protocol. The main goal of this audit is to validate that these properties and their proofs apply to the code produced.

We will first describe the artifacts in the scope of this audit before explaining the specific statements we want the auditor to assess.

![artifacts.png](artifacts.png)

TODO: describe hypothesis / security definition / environments (e.g. API is used locally)..


### Artifact 1: Original publication

The Hydra head protocol has first been published in [Hydra: Fast Isomorphic State Channels](https://eprint.iacr.org/2020/299.pdf). This paper describes several versions of the protocol (simple, with or without conflict resolution, incremental (de)commits, etc.). And provides the corresponding security proofs, especially that the following four properties hold:

* Consistency: No two uncorrupted parties see conflicting transactions confirmed.
* Liveness: If all parties remain uncorrupted and the adversary delivers all messages, then every transaction becomes confirmed at some point.
* Soundness: The final UTxO set accepted on the mainchain results from a set of seen transactions.
* Completeness: All transactions observed as confirmed by an honest party at the end of the protocol are considered on the mainchain.

A study of the whole paper and possible variations of the protocol is out of scope of this solicitation, but it serves as a good starting point and introduction to the overall protocol. Note that the implemented Hydra Head protocol - name **Coordinated Hydra Head** - can be considered a subset of the "Simple Protocol without Conflict Resolution". That is, we recommend reading Chapters 2-6, where the off-chain "Head protocol" is different in the actual specification.

### Artifact 2: Coordinated Hydra Head V1 Specification

The Hydra Head protocol implementation derives from the original publication in several ways. Especially some simplification have been introduced and generalizations removed.

The [Coordinated Hydra Head V1 specification](https://docs.google.com/document/d/1XQ0C7Ko3Ifo5a4TOcW1fDT8gMYryB54PCEgOiFaAwGE/) captures these deviations and also includes the "formal notation" of the actual transaction constraints (which are foregone in the original paper). Also, it details the L2 protocol logic for the **Coordinated** Head protocol - which is implemented in V1.

Note that it is lacking some structure and introductory sections and we recommend to see Artifact 1 for that.

FIXME the following list is probably not usefull since it should be in the spec
In particular, the following simplifications are done in the actual implementation:
* Kagg is a list of keys
* snapshot is signed for every transaction and only snapshot can be used in close transaction
* ...

### Artifact 3: Hydra Head Protocol Implementation

With Hydra Head Protocol Implementation we refer to the software component that is used to operate a node in the Hydra Head protocol. The `hydra-node` allows its users to open a head, lock funds in it, connect to peers, process transactions as a layer 2, close a head and unlock the corresponding funds. It includes Hydra plutus scripts, Hydra head chain layer, layer 2 code, network communication between peers, and an API for clients to connect and use the node.

The implementation can be found in this [Github repository](https://github.com/input-output-hk/hydra-poc)

Version to be audited: [0.9.0](https://github.com/input-output-hk/hydra-poc/releases/tag/0.9.0)

TODO describe the inputs and outputs of a hydra node

TODO: clarify which artifacts we need to introduce

TODO:We should share our test architecture/topology and also the test results here: https://hydra.family/head-protocol/benchmarks/tests/hydra-cluster/hspec-results

#### Artifact 3.3: Hydra plutus scripts

TODO

Grab stuff from https://hydra.family/head-protocol/haddock/hydra-plutus/index.html

#### Artifact 3.4: Hydra node chain layer code

TODO

Grab stuff from https://hydra.family/head-protocol/haddock/hydra-node/index.html sub-sections of _Hydra.Chain.Direct_

#### Artifact 3.5: layer 2 code

TODO

## Tasks

On a broad level, our goal is to ensure that the security properties proven in the original publication hold for the implementation, taking also into consideration the main entry points of a hydra node which are the network, the API and the Cardano ledger.

TODO: say something about increasing confidence to our users in using the hydra-node and the protocol implemented by it;


Given the artifacts described above, 
We expect the auditor to assess several statements which will be detailed in the following sections.

By order of priority:
-- pen and paper stuff
1. Coordinated Hydra Head V1 specification is consistent with the original publication

TODO: ask Yun what she thinks about that

--plutus stuff

2. Hydra plutus scripts are consistent with Hydra Head v1 specification
3. Hydra plutus scripts are immune to common Cardano smart-contract weaknesses

-- hydra node implementation is behaving as an honest actor (out of memory denial of service...)

4. Hydra node chain layer code generates transactions which are consistent with Hydra Head v1 specification
5. Hydra node logic layer code is consistent with Hydra Head v1 specification

-- robust against the environment whatever

6. Hydra head protocol implementation is immune to attacks via chain transactions
7. Hydra head protocol implementation is immune to attacks via network
8. Hydra head protocol implementation faithfully reflects the head state through the API

Out os scope:
1. Hydra head protocol implementation is immune to API attacks -- out of scope because trusted

FIXME: split the RFP or give the option to respond partially. Ask for a detailed estimation of each of the tasks so that we may choose to not ask for everything.

### Coordinated Hydra Head V1 specification is consistent with the original publication

The Hydra Head v1 specification describes Coordinated Hydra Head V1 Protocol.

This specification provides several important security proofs, especially that the following four properties hold:

* Consistency: No two uncorrupted parties see conflicting transactions confirmed.
* Liveness: If all parties remain uncorrupted and the adversary delivers all messages, then every transaction becomes confirmed at some point.
* Soundness: The final UTxO set accepted on the mainchain results from a set of seen transactions.
* Completeness: All transactions observed as confirmed by an honest party at the end of the protocol are considered on the mainchain.

You will review this specification to give us comments and assess that the above properties hold. The outcome of the review should include, but not being limited to:
* Identification of any inconsistencies or lack of generality within the specification;
* Identification of any inconsistencies in the proofs exposed in the specification;
* Identification of any behavior that could lead, with an adverserial mindset, to one of the above properties to be falsified.

TODO: should we ask, and how, the auditor to provide feedback about the Hydra Head v1 specification on clarity, ambiguity, readability and comprehensibility; it is fit to serve as a foundation for the implementation of the protocol?

FIXME: v1 version of the specification with proof needed.

### Hydra plutus scripts are consistent with Hydra Head v1 specification

The Coordinated Hydra Head V1 specification defines the controls the on-chain validators must implement for a functionning hydra head.

You will review the corresponding artifacts described above to assess that the Hydra plutus scripts are consistent with the Coordinated Hydra Head V1 specification. The outcome of the review should include, bo not being limited to:
* a review and comment of the mutation based tests applied to the Hydra plutus scripts and, in particular, any adversarial situation that would not be covered by them but should be in the context of this audit;
* a validation that the Hydra plutus scripts validators do check the transaction constraints defined in the Coordinated Hydra Head V1 specification;
* any comment about practical Cardano smart contract issue, absent form the specification, which would not be handled by the Hydra plutus scripts;

TODO: should we ask, and how, the auditor for feedbacks about potential scripts optimizations?

### Hydra plutus scripts are immune to common Cardano smart-contract weaknesses

TODO see section 4.3 of Marlowe's RFP

Evaluate the Hydra Head protocol implementation validators' susceptibility to common possible vulnerabilities for Cardano smart contracts,, such as but not limited to the following attacks:
* Execution cost limits;
* Double satisfaction;
* Replay;
* Denial of service.

See, for example, https://github.com/Plutonomicon/plutonomicon/blob/main/vulnerabilities.md.

See the draft https://plutus--4604.org.readthedocs.build/en/4604/reference/common-weaknesses/index.html.

### Hydra node chain layer code generates transactions which are consistent with Hydra Head v1 specification

TODO
- The off-chain code creates transactions as specified (TODO: which modules / packages are relevant? how much code is this?)

### Hydra node logic layer code is consistent with Hydra Head v1 specification

TODO
- The L2 code handles chain & network events as specified

### Hydra head protocol implementation is immune to attacks via chain transactions

TODO

An attacker posts transactions on chain which mess with our implementation.
- Do perform some attacks
- Review the testing strategy (i.e. mutation tests)
- ...

Deny of Service with wrongful initialilizeHead transaction

### Hydra head protocol implementation is immune to attacks via network

TODO

Define some security hypotheses regarding network connections and check that, under these hypotheses, our code prevent an attacker to mess with the head

Denial of service through the network (contesting a close becoming impossible, etc.)

### Hydra head protocol implementation faithfully reflects the head state through the API

TODO

We don't care about being attacked through the API.

If we state that API is only accessible through local loopback then it's probably just an obvious demonstration here. But it might lead to interesting discoveries like what installation instructions we share with the users to optimize for their security.

## Out of Scope

The scope of this audit has been described in the above sections. What is not in scope is out of scope. In particular, the following items is out of scope of this audit.

Verify the whole original paper and its proofs.

1. Hydra head protocol implementation is immune to API attacks -- out of scope because trusted
