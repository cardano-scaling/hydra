# Hydra development

This guide is meant to be a tutorial on how to develop an application on Hydra. It will assume the reader is already familiar with developing a DApp on Cardano and will focus on the differences between the two.

## On-chain code

On-chain code will be exactly the same between Cardano and a hydra head. This is one of the main selling points of the hydra design (heads are also called isomorphic state channels) the only caveat being that (at the time of writing 07/22) Hydra Heads do not support validity ranges for transactions, the reason for this is there is currently no notion of time inside of a Hydra Head. See also this [feature on the roadmap](https://github.com/input-output-hk/hydra-poc/issues/196)

## Off-chain code

Here is where the differences will begin. Currently, there is no [support for the PAB](https://github.com/input-output-hk/hydra-poc/issues/214). When it comes to hydra, the luxaries of the `Contract` monad are not available, so the developer will not have access to similar tools.

Instead, transactions will need to be constructed and submitted to the Hydra Node in CBOR format. The interface to speak to a Hydra Node is a websocket, and developers must build applications that connect to these websockets in order to send and receive events through them.

The Hydra node will emit several commands through this websocket: the API is defined [here](https://hydra.family/head-protocol/api-reference).
The API reference is divided into two sections: Pub and Sub.
The first one describes the commands that a hydra node will accept, while the second one describes the events that will be emitted through the websocket.

An important event developers will be interested in is `SnaphotConfirmed`. This event is emitted once all the participants of the head, have seen and signed a transaction submitted by one of them.

Once a transaction lands inside a snapshot, it is confirmed and there are no chances for it to be rolled back. This is an important distinction from L1: in that case, when a node sees a transaction there is still a chance that it might be rolled back (where the probability of this goes to 0 as more blocks are added), in the hydra world, the only way to make a snapshot is to have every single node in the head approve of it.
This is a synchronous process and requires all the head participants to be online all the time, the tradeoff is that there is no uncertainty or possibility of rollbacks. Once the Hydra Head has been open long enough that the opening transaction has become final, the moment the snapshot is confirmed there is no going back. See also this [feature on the roadmap](https://github.com/input-output-hk/hydra-poc/issues/185).

For the Pub side of the API, an important endpoint will be `NewTx`. This is used to submit a transaction to the node. There are a couple of ways the transaction can be submitted, but a simple and familiar way should be to build the transaction built with `Cardano.Api`, sign & serialize it to CBOR and submit that through the websocket.

## Testing

Testing inside the hydra head can be quite different than for working with the PAB, the reason is you will not have access to EmulatorTrace or a pre-deployed playground environment to test.

Instead, it is possible to spin up a local cardano devnet, as well as and a cluster of hydra-nodes programmatically and run tests directly on that.
The hydra-cluster package is used within Hydra to do end-to-end tests on the protocol and hence is a nice blueprint for this setup. It's library component provides functions to spin up the required nodes, send API inputs and run assertions on the API output. Should you be missing something or need more configurability, feel free to open a feature request and we are happy to support you.

A couple of steps are required to set up the initial environment:

- We want to spin up a local Cardano devnet comprised by a single block producer. This can be done with `withCardanoDevnet` which takes the a working directory where blocks and logs will be stored, as well as a callback that gets access to the `RunningNode`. Note that the devnet will use credentials and configuration from the `hydra-cluster/config/`.

- In the callback, we can then spin up a cluster of hydra-nodes using `withHydraCluster`. This will require the cardano-node socket, which can be obtained from the running node instance, and a set of cardano and hydra keys which can also be easily generated through helpers exposed by `hydra-cluster`.

Let's walk through a simple example, with comments explaining the setup we just described.
This test will spin up a single cardano node, and a pair of hydra nodes (Alice and Bob).

```haskell
-- Tracer is an instance of https://hackage.haskell.org/package/contra-tracer and is used by several
-- hydra components to deal with logs in a structured way
it "spins up two hydra nodes" -> do
  let tracer = stdoutTracer
  -- Set a timeout for the whole test to fail
  failAfter 60 $
    -- We create a temporary directory to contain all the files required to spin up a node
    withTempDir "cardano-node-tmp-dir" $ \tmpDir -> do
      -- This starts the cardano devnetas described above
      withCardanoDevnet (contramap FromCardanoNode tracer) tmpDir $ \(RunningNode{nodeSocket}) -> do
        -- We generate pairs of cardano keys for alice and bob, this pair is a tuple of verification and signing key
        aliceKeys <- generate genKeyPair
        bobKeys <- generate genKeyPair

        -- Here we generate signing keys for hydra used to sign transactions on layer 2 only
        let aliceSk = generateSigningKey "alice"
            bobSk = generateSigningKey "bob"

        let cardanoKeys = [aliceKeys, bobKeys]
            hydraKeys = [aliceSk, bobSk]

        -- Hydra nodes take an id as one of their arguments, it is used to identify them with the head protocol
        let firstNodeId = 0

        -- This will spin up `n` hydra nodes, where `n` is the length of the cardanoKeys list
        -- (which needs to match the length of hydraKeys)
        withHydraCluster tracer tmpDir nodeSocket firstNodeId cardanoKeys hydraKeys $ \nodes -> do
          pure ()
```

At this point, we are ready to start sending commands to the `hydra-cluster` programmatically.
Hydra-cluster also exposes a `send` function, which takes an instance of the hydra-node and uses its websocket connection to send commands to the process.
Remember that we must handle the whole head initialisation inside the test as well, so the first step will always be for one of the two nodes to `send` an `Init` command to start the opening of the head.

Finally, hydra-node exposes several useful functions to wait for output from the hydra nodes themselves, both these functions have a timeout so they will function as assertions from a testing point of view; that is, if the node does not output what we expect within a certain timeframe, an error will be thrown and the whole test will fail.

Remember the nodes will always produce output in JSON format, so all the assertions will expect some form of `Aeson.Value` to check against. For example, after we submit a transaction to the node, we can use `waitMatch` to parse the `SnaphotConfirmed` event and extract the transactions present in that snapshot to check if it contains the transaction we just sent.

For more details about the `hydra-cluster` functions (i.e. `waitFor` and `waitMatch`), visit the [haddock](https://hydra.family/head-protocol/haddock/hydra-cluster/HydraNode.html)
