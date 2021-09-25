This is our standard demo setup for demonstrating the Hydra Head protocol.

It consists of
- a cluster of three `hydra-node`s, directly connected to each other, each having access to one of three credentials `alice`, `bob` or `carol`
- a `mock-chain` also connecting all nodes
- a `prometheus` server gathering metrics
- ad-hoc `hydra-tui` clients to interact with the individual `hydra-node`s

Using [`docker-compose`](https://docs.docker.com/compose/) you can start the demo cluster with:
```sh
docker-compose pull
docker-compose up -d
```
And attach three `hydra-tui` clients to each `hydra-node` (in three terminals):

``` sh
docker run --rm it inputoutput/hydra:hydra-tui-latest --connect localhost:4001 # alice's hydra-node
docker run --rm it inputoutput/hydra:hydra-tui-latest --connect localhost:4002 # bob's hydra-node
docker run --rm it inputoutput/hydra:hydra-tui-latest --connect localhost:4003 # carol's hydra-node
```
