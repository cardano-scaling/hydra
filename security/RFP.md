See [CIP-52](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0052)

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

# Scope of the audit

We provide the auditor with three artefact:
* Hydra original paper
* Hydra implementation specification
* Hydra on-chain code base
* Hydra off-chain code base
* Hydra layser 2 code base

We expect the auditor to assess these three statements:
* Hydra implementation specification is compliant with the Hydra original paper to the extent that the proofs in the Hydra original paper apply to the Hydra implementation specification.
* Hydra on-chain and off-chain code base is compliant with Hydra implementation specification
* Hydra layer 2 code base is compliant with Hydra implementation specification
* Given the security hypotheses, Hydra layer 2 code base is immune to side channel attacks through the network or the API or impersonating a user or any other attack you can think of.
