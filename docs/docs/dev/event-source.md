## Event sourcing

The `hydra-node` is an event sourced application. This means that the main logic is processing _inputs_ (also called commands) and produces _events_. These events are saved and loaded to persist application state across restarts. Also, most events are transformed to _outputs_ and can be observed on the `hydra-node` API.

On application startup, the [`hydrate`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Node.html#v:hydrate) function is called to load all events using a given [`EventSource`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Events.html#t:EventSource) and while doing so, re-emits th events to all provided [`EventSink`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Events.html#t:EventSink) instances. The resulting [`HydraNode`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Node.html#t:HydraNode) will then enter the main loop of `hydra-node` and process inputs into state changes and effects via function [`stepHydraNode`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Node.html#v:stepHydraNode). All state changes of a Hydra node are based on [`StateEvent`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Events.html#t:StateEvent) values and consequently get emitted to all `eventSinks` of the `HydraNode` handle. Also, the `eventSource` of the same may be used later to to load events on-demand, for example to produce a history of server outputs.

- TODO: explain `StateEvent` data type and `EventSource` / `EventSink` interfaces

## Default event source and sinks

The default event source and sink used by the `hydra-node` is `FileBased`, which uses an append-only plain JSON file to persist events in a file name `state`. This single file is located in the `hydra-node` persistence directory, which is specified by the `--persistence-dir` command line option. 

As explained in the consequences of [ADR29](https://hydra.family/head-protocol/adr/29), the API server of the `hydra-node` is also an event sink, which means that all events are sent to the API server and may be further submitted as [`ServerOutput`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-API-ServerOutput.html#t:ServerOutput) to clients of the API server. See [`mkTimedServerOutputFromStateEvent`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-API-Server.html#v:mkTimedServerOutputFromStateEvent) for which events are mapped to server outputs.

## Examples

Besides the efault event source and sinks, there are two examples in the `hydra-node:examples` library:

- [`Hydra.Events.UDP`](https://github.com/cardano-scaling/hydra/blob/master/hydra-node/examples/Hydra/Events/UDP.hs): A simple UDP sink that sends all events to a UDP socket. This is a simple example of an event sink that can be used to send events to other applications or services.

- [`Hydra.Events.S3`](https://github.com/cardano-scaling/hydra/blob/master/hydra-node/examples/Hydra/Events/S3.hs): This example shows how to use AWS S3 to create a complete event store - that is, an event source and sink. It uses the `amazonka` to interact with S3 and store events in a bucket. This is a more complex example that demonstrates how to use an external service to store events.

### What to test

When implementing an event source or sink, you might want to consider testing the following, as also demonstrated by the main implementations and examples above:

- [ ] Event store (sink + source)
  - [ ] Completeness: Any events stored by `putEvent` are returned by `getEvents`/`sourceEvents`
    - [ ] For continuous, non-continuous and sequences of duplicate events
  - [ ] Concurrent use of `putEvent` and `sourceEvents` is possible

- [ ] Event sink only
  - [ ] Whether `putEvent` results in desired effect
  - [ ] Concurrent use of `putEvent` is possible
  
- [ ] Event source only
  - [ ] Whether previously "primed" events are loaded by `sourceEvents`
  - [ ] Concurrent use of `sourceEvents` is possible
  
- [ ] General: allocated resources are released (use with/bracket pattern)
