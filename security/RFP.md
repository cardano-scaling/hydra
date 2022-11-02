See [CIP-52](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0052)

# *Specification / design documents*

> Submitters shall provide specification and design documents that describe in a precise and unambiguous way the architecture, interfaces and requirements characterising the DApp. 

> The documentation shall identify the expected behaviour of the code, given without direct reference to the code itself. The description should also include high-level examples demonstrating use cases of the DApp. All assumptions about how the DApp will be used will be described. The documentation shall identify and document all the interfaces with other components and services.

> Submitters might also wish to explain mitigating actions that they have taken to protect against potential failures and attacks to the DApp.

Grab stuff from https://hydra.family/head-protocol/core-concepts

Implementation of a subset of [Hydra: Fast Isomorphic State Channels](https://eprint.iacr.org/2020/299.pdf)
In particular, the following simplifications are done in the actual implementation:

* Kagg is a list of keys
* snapshot is signed for every transaction and only snapshot can be used in close transaction
* ...


# *On-Chain Specification* 

> The format of transactions accepted by the smart contracts should be specified using the template provided in the auxiliary document `Tx-spec.md`.

> The document should clearly specify the properties to be satisfied by the smart contract.
> * Properties shall be as extensive as possible and ideally would cover functionality, robustness, safety, liveness and efficiency, e.g. cost of execution, of the smart contract. 
> * Discussion should describe whether any of the properties addresses common vulnerabilities pertaining to Cardano blockchain or the smart contract domain in general. 

> A formal specification is recommended but not mandatory. 

Grab stuff from https://hydra.family/head-protocol/haddock/hydra-plutus/index.html


# *Off-Chain Specification*

> For off-chain analysis additional information should be provided for the components and services interfaced:
> * For all interfacing components, a specification shall be given detailing their expected behaviour in relation to the DApp, including any assumptions, constraints and requirements of the component that are expected to hold by the DApp developers.
> * It also shall be stated whether any of the interfacing components have been certified.

Grab stuff from https://hydra.family/head-protocol/haddock/hydra-node/index.html sub-sections of _Hydra.Chain.Direct_

# *Layer 2 Specification*

We should describe here what is layer 2 in Hydra. The fact that we run transactions, sign them together (snapshots) and the fact that we communicate on the network to do so.


# *Testing*

> Ideally, submitters should submit a description of how the DApp has been tested, the results of the tests, and details of how those test results can be replicated.In particular:
> * The test cases and their results shall be recorded or reproducible in a machine-readable format to facilitate subsequent analysis.
> * Tests are to be performed for each targeted platform (browser, wallet etc).
> * The identity, configuration and version of all test components involved shall be documented.
> * The checksum and version of the DApp submitted for certification shall correspond to the same version making the subject of the test report. 
> * An evaluation of the test coverage and test completion should be provided. 


> In the case that off-chain code is included in the scope of the audit, testing should be able to assess the performance and robustness of the DApp against significant throughput, under substantial workload, and in the scenario of a DoS attack.

We should share our test architecture/topology and also the test results here: https://hydra.family/head-protocol/benchmarks/tests/hydra-cluster/hspec-results

# *Source code and version*

> A final version of the source code should be provided that works with the use cases specified in the documentation. Information needs to be provided to allow the DApp to be built in an unambiguous and reproducible way, including any external components and services that the DApp uses.  This could be in the form of


> * The URL for a commit to a repository.
> * Build information for the DApp: a pure nix build is particularly suitable, since this will identify versions of  libraries, compilers, OS, etc.
> * For the on-chain code for a DApp, the specific contracts to be audited.

Assessment should be performed for version 0.9.0 of hydra-poc code.

# *Versioning*

> Versioning information needs to be given in a way that allows end users of a DApp to determine whether or not the version of the DApp that they are using is covered by certification information held on blockchain.


> This can be done in a number of different ways, depending on the type of audit. These include:
> 1. The hash of a URL for a commit to a publicly-available repository.
> 2. A hash that identifies the files that contain the on-chain code that has been audited, e,g computing, from the root of the repository, listed in lexicographic order.

# *Registration*

> It is planned that DApps will be registered on the Cardano blockchain. This is currently under discussion. Once that discussion has been settled, it will also be possible to provide on-chain evidence of audit, linked to a registered entity. The mechanism for this is described in a separate document which it is intended to make into another CIP. A current draft of that document is here: [Proof of Audit document](https://docs.google.com/document/d/1FvgX8QiGKVPv4c7HanZ92zwstD9U1amOf8eHvyIb1dI).

