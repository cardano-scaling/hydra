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

FIXME the following list is probably not usefull since it should be in the spec
In particular, the following simplifications are done in the actual implementation:
* Kagg is a list of keys
* snapshot is signed for every transaction and only snapshot can be used in close transaction
* ...



### Artifact 3: Hydra Head Protocol Implementation

The Hydra Head Protocol Implementation is the software that is used to operate a node in a head. It allows its users to open a head, lock funds in it, connect to peers, perform transaction in a layer 2 environment, close a head and unlock the corresponding funds. It includes on-chain code, off-chain code, layer 2 code, network communication between peers, API for clients to connect and use the node.

TODO describe the inputs and outputs of a hydra node

#### Artifact 3.3: on-chain code

TODO

Grab stuff from https://hydra.family/head-protocol/haddock/hydra-plutus/index.html

#### Artifact 3.4: off-chain code

TODO

Grab stuff from https://hydra.family/head-protocol/haddock/hydra-node/index.html sub-sections of _Hydra.Chain.Direct_

#### Artifact 3.5: layer 2 code

TODO

## Tasks

On a broad level, our goal is to ensure that the security properties proven in the original publication hold for the implementation, taking also into consideration the main entry points of a hydra node which are the network, the API and the Cardano ledger.


Given the artifacts described above, 
We expect the auditor to assess the following statements.

### Hydra Head v1 Formal Specification is sound with the original publication

TODO

Hydra implementation specification is compliant with the Hydra original paper to the extent that the proofs in the Hydra original paper apply to the Hydra implementation specification.

### Hydra head protocol implementation is sound with Hydra implementation specification

TODO

### Hydra head protocol implementation is immune to on-chain attacks

TODO

An attacker posts transactions on chain which mess with our implementation.

### Hydra head protocol implementation is immune to network attacks

TODO

Define some security hypotheses regarding network connections and check that, under these hypotheses, our code prevent an attacker to mess with the head

### Hydra head protocol implementation is immune to API attacks

TODO

If we state that API is only accessible through local loopback then it's probably just an obvious demonstration here. But it might lead to interesting discoveries like what installation instructions we share with the users to optimize for their security.

## Out of Scope

The scope of this audit has been described in the above sections. What is not in scope is out of scope. In particular, the following items is out of scope of this audit.

Verify the whole original paper and its proofs.

---
DO NOT READ AFTER THIS LINE
---

# Submission

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
