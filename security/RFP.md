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

The Hydra head protocol implementation is composed of several parts, on-chain code, off-chain code, layer 2 code and this implementation is based on general and specific specification which contain proofs of several properties of the protocol. The main goal of this audit is to validate that these properties and their proofs apply to the code produced.

We will first describe the artifacts in the scope of this audit before explaining the specific statements we want the auditor to assess.

![artifacts.png](artifacts.png)

### Artifact 1: Original publication

The Hydra head protocol has first been published in [Hydra: Fast Isomorphic State Channels](https://eprint.iacr.org/2020/299.pdf). This paper describes several versions of the protocol (simple, with or without conflict resolution, incremental (de)commits, etc.). And provides the corresponding security proofs, especially that the following four properties hold:
* Consistency: No two uncorrupted parties see conflicting transactions confirmed.
* Liveness: If all parties remain uncorrupted and the adversary delivers all messages, then every transaction becomes confirmed at some point.
* Soundness: The final UTxO set accepted on the mainchain results from a set of seen transactions.
* Completeness: All transactions observed as confirmed by an honest party at the end of the protocol are considered on the mainchain.

The Hydra Head protocol implementation should be considered as a subset of the Simple Protocol without Conflict Resolution.

A study of the whole paper and possible variations of the protocol is out of scope of this solicitation.

### Artifact 2: Hydra Head v1 Formal Specification

The Hydra Head protocol implementation derives from the original publication in several ways. Especially some simplification have been introduced and generalizations removed.

Hydra Head v1 Formal Specification captures these deviations in a formal specification.

### Artifact 3: Hydra Head Protocol Implementation

The Hydra Head Protocol Implementation is the software that is used to operate a node in a head. It allows its users to open a head, lock funds in it, connect to peers, perform transaction in a layer 2 environment, close a head and unlock the corresponding funds. It includes on-chain code, off-chain code, layer 2 code, network communication between peers, API for clients to connect and use the node.

TODO describe the inputs and outputs of a hydra node

#### Artifact 3.3: on-chain code

TODO

#### Artifact 3.4: off-chain code

TODO

#### Artifact 3.5: layer 2 code

TODO

## Tasks

Given the artifacts described above, 
We expect the auditor to assess the follwing statements.

### Hydra Head v1 Formal Specification is sound with the original publication

TODO

Hydra implementation specification is compliant with the Hydra original paper to the extent that the proofs in the Hydra original paper apply to the Hydra implementation specification.

### Hydra on-chain and off-chain code base is compliant with Hydra implementation specification

TODO

### Hydra layer 2 code base is compliant with Hydra implementation specification

TODO 

### Given the security hypotheses, Hydra layer 2 code base is immune to side channel attacks through the network or the API or impersonating a user or any other attack you can think of.

TODO

## Out of Scope

The scope of this audit has been described in the above sections. What is not in scope is out of scope. In particular, the following items is out of scope of this audit.

Verify the whole original paper and its proofs.

---
DO NOT READ AFTER THIS LINE
---

# Submission

## *Specification / design documents*

> Submitters shall provide specification and design documents that describe in a precise and unambiguous way the architecture, interfaces and requirements characterising the DApp. 

> The documentation shall identify the expected behaviour of the code, given without direct reference to the code itself. The description should also include high-level examples demonstrating use cases of the DApp. All assumptions about how the DApp will be used will be described. The documentation shall identify and document all the interfaces with other components and services.

> Submitters might also wish to explain mitigating actions that they have taken to protect against potential failures and attacks to the DApp.

Grab stuff from https://hydra.family/head-protocol/core-concepts

Implementation of a subset of [Hydra: Fast Isomorphic State Channels](https://eprint.iacr.org/2020/299.pdf)
In particular, the following simplifications are done in the actual implementation:

* Kagg is a list of keys
* snapshot is signed for every transaction and only snapshot can be used in close transaction
* ...


## *On-Chain Specification* 

> The format of transactions accepted by the smart contracts should be specified using the template provided in the auxiliary document `Tx-spec.md`.

> The document should clearly specify the properties to be satisfied by the smart contract.
> * Properties shall be as extensive as possible and ideally would cover functionality, robustness, safety, liveness and efficiency, e.g. cost of execution, of the smart contract. 
> * Discussion should describe whether any of the properties addresses common vulnerabilities pertaining to Cardano blockchain or the smart contract domain in general. 

> A formal specification is recommended but not mandatory. 

Grab stuff from https://hydra.family/head-protocol/haddock/hydra-plutus/index.html


## *Off-Chain Specification*

> For off-chain analysis additional information should be provided for the components and services interfaced:
> * For all interfacing components, a specification shall be given detailing their expected behaviour in relation to the DApp, including any assumptions, constraints and requirements of the component that are expected to hold by the DApp developers.
> * It also shall be stated whether any of the interfacing components have been certified.

Grab stuff from https://hydra.family/head-protocol/haddock/hydra-node/index.html sub-sections of _Hydra.Chain.Direct_

## *Layer 2 Specification*

We should describe here what is layer 2 in Hydra. The fact that we run transactions, sign them together (snapshots) and the fact that we communicate on the network to do so.


## *Testing*

> Ideally, submitters should submit a description of how the DApp has been tested, the results of the tests, and details of how those test results can be replicated.In particular:
> * The test cases and their results shall be recorded or reproducible in a machine-readable format to facilitate subsequent analysis.
> * Tests are to be performed for each targeted platform (browser, wallet etc).
> * The identity, configuration and version of all test components involved shall be documented.
> * The checksum and version of the DApp submitted for certification shall correspond to the same version making the subject of the test report. 
> * An evaluation of the test coverage and test completion should be provided. 


> In the case that off-chain code is included in the scope of the audit, testing should be able to assess the performance and robustness of the DApp against significant throughput, under substantial workload, and in the scenario of a DoS attack.

We should share our test architecture/topology and also the test results here: https://hydra.family/head-protocol/benchmarks/tests/hydra-cluster/hspec-results

## *Source code and version*

> A final version of the source code should be provided that works with the use cases specified in the documentation. Information needs to be provided to allow the DApp to be built in an unambiguous and reproducible way, including any external components and services that the DApp uses.  This could be in the form of


> * The URL for a commit to a repository.
> * Build information for the DApp: a pure nix build is particularly suitable, since this will identify versions of  libraries, compilers, OS, etc.
> * For the on-chain code for a DApp, the specific contracts to be audited.

Assessment should be performed for version 0.9.0 of hydra-poc code.

## *Versioning*

> Versioning information needs to be given in a way that allows end users of a DApp to determine whether or not the version of the DApp that they are using is covered by certification information held on blockchain.


> This can be done in a number of different ways, depending on the type of audit. These include:
> 1. The hash of a URL for a commit to a publicly-available repository.
> 2. A hash that identifies the files that contain the on-chain code that has been audited, e,g computing, from the root of the repository, listed in lexicographic order.

## *Registration*

> It is planned that DApps will be registered on the Cardano blockchain. This is currently under discussion. Once that discussion has been settled, it will also be possible to provide on-chain evidence of audit, linked to a registered entity. The mechanism for this is described in a separate document which it is intended to make into another CIP. A current draft of that document is here: [Proof of Audit document](https://docs.google.com/document/d/1FvgX8QiGKVPv4c7HanZ92zwstD9U1amOf8eHvyIb1dI).


# Requirements for Auditors

## *Responsibilities*

Auditors shall be able to carry out the following activities: 
* Review the requirement specification document against the intended environment and use so as to:
   * Identify any inconsistencies, security flaws or incomplete requirements
   * Identify any implicit assumptions and whether they are justifiable or not
   * Evaluate the adequacy of strategies applied by the submitter to guarantee the consistency, correctness and completeness of the requirements
   * Identify a threat model to guarantee that any identified mitigations are indeed appropriate against a list of possible vulnerabilities for Cardano smart contracts, and which is currently being finalised.
*  The source code shall be audited by manual and/or automated means. In particular,
   * The source code shall be reviewed against the requirements to ensure that all of these are properly taken into account and completely fulfilled.
   * The adequacy of the source code documentation and traceability with the requirements shall be assessed.
   * The source code shall be free from coding patterns/programming mistakes that may introduce exploitable vulnerabilities/failures leading to security issues.
* Produce a detailed audit report describing scope, methodology, and results categorised by severity. In particular,
   * Any discrepancies, deviations or spotted vulnerabilities shall be described and classified with an appropriate severity level. Recommendations to rectify the identified deficiencies shall also be provided whenever appropriate.
   * When automated tools are used as a replacement for manual review/code inspection, they shall be documented or referenced. Note that it’s the responsibility of the auditor to ensure that such tooling may not exhibit potential failures that can adversely affect the review outcome.
   * Any strategies/methodologies used to assess the consistency, correctness and completeness of the requirements shall also be documented or referenced.

## *Key competencies*

Auditors shall provide credentials for the following competencies:
* They shall have an in-depth knowledge of the syntax and semantics of the smart contract language to be audited, the underlying blockchain technology and associated computation and cost models.
* They shall be competent in the strategies and methods used to elaborate threat models.
* They shall be competent in assessing the suitability of methods (or combination of methods) used to justify the consistency, correctness and completeness of requirements against the list of common vulnerabilities pertinent to the smart contract domain and to guarantee (as far as possible) the absence of security flaws in the design.
* They shall be competent in various test and verification methods and have solid background in the various test coverage criteria (i.e., statement, data flow, branching, compound condition, MC/DC and Path).
* They shall also be able to assess whether the set of test cases produced for each specific test objective/property are sufficient enough to cover all the possible functional cases.
* They shall have analytical and critical thinking ability pertaining to the:
   * deployment and execution of smart contracts on the underlying blockchain technology;
   * Potential attacks or sequence of events relative to the smart contract’s logic that may lead to an unsafe state or invalidate some of the fundamental properties of the contract.
* They shall be able to judge the adequacy of the justifications provided by submitters w.r.t., development processes (e.g., requirement elicitation techniques, threat models, test objectives and test cases, coding standard, quality management, etc) for Level 2 certification.

## *Disclosure*
Disclosure
It is common – but not universal – practice for disclosure/publication of audit report, for example as a part of a responsible disclosure policy. A typical policy would be to publish a report after a certain period (e.g. 30-90 day) or at the point that a DApp goes live, whichever is earlier.


