# local-cluster

Command line executable and integration test suite spinning up a local cluster of Cardano and Hydra nodes.

This is used to experiment and explore the design space for the Hydra node, how
it interacts with Cardano nodes and have an always available, ad-hoc environment
for demos.

## Scenario

For now, we use a static scenario in which we first develop the Hydra node:

- 1 Cardano BFT node
- sped up network parameters
- directly forks into Allegra and Mary
- 3 Cardano stake pool nodes
