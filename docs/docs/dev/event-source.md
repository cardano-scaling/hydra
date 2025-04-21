## Event sourcing

The `hydra-node` is an event sourced application. This means that the main logic is processing _inputs_ (also called commands) and produces _events_. These events are saved and loaded to persist application state across restarts. Also, most events are transformed to _outputs_ and can be observed on the `hydra-node` API.

On application startup, the [`hydrate`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Node.html#v:hydrate) function is called to load all events using a given [`EventSource`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Events.html#t:EventSource) and while doing so, re-emits th events to all provided [`EventSink`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Events.html#t:EventSink) instances. The resulting [`HydraNode`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Node.html#t:HydraNode) will then enter the main loop of `hydra-node` and process inputs into state changes and effects via function [`stepHydraNode`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Node.html#v:stepHydraNode). All state changes of a Hydra node are based on [`StateEvent`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Events.html#t:StateEvent) values and consequently get emitted to all `eventSinks` of the `HydraNode` handle. Also, the `eventSource` of the same may be used later to to load events on-demand, for example to produce a history of server outputs.

- TODO: explain `StateEvent` data type and `EventSource` / `EventSink` interfaces

## Default event source and sinks

TODO: explain
- FileBased source + sink
- API server as sink

## Extending the node via event sources and sinks

TODO: list / describe examples 
- UDP sink
- S3 source and sink

TODO: considerations when implementing a source / sink
- multi-threading
- what to test 

---

TODO: re-use parts as suitable


There are certain use cases in which multiple features of the Hydra platform are beneficial. However, interfacing with the entire `hydra node`, particularly with regard to IO, may be impractical. For a use case that requires different persistence requirements than the default Hydra setup, it may initially appear that there are two options available:

## Deciding between forking and using event sinks

- **Forking the Hydra codebase**. Customizing the Hydra codebase can provide precise control over node behavior but introduces a significant maintenance burden due to the instability and lack of internal code documentation.

- **Using event sinks and source**. Running a full `hydra node` requires preparing a persistence file beforehand or parsing the file as it is written. However, this approach has downsides, including reduced control, code duplication, increased resource usage, and reliance on interfacing with an unstable external API (the persistence file on disk).

## Implementation of event sinks and source

Hydra introduces alternate event sinks and a single alternate event source to enhance these use cases. Event sinks permanently store new transactions processed at runtime, while the event source provides the initial transactions that a `hydra node` loads upon startup.

Multiple event sinks can be utilized simultaneously, whereas only one event source can be used at a time. The event source is loaded only during startup. Each event sink runs for every new transaction. Currently, the order of the event sinks must be specified by customizing the order of the event sink list in the `hydra node` source code, in the `eventSinks` parameter to hydrate, invoked in `Hydra.Node.Run.run` [here](https://github.com/SundaeSwap-finance/hydra/blob/4785bd86a03b92ba8fa8fb34c9d485a1e2f4f7d7/hydra-node/src/Hydra/Node/Run.hs#L104).

The default Hydra file-based persistence is implemented as an event sink and source pair. They can be used independently. For example, you can use the default event source to process previous transactions from a file on disk, along with an event sink that could store new transactions on S3, on several machines, or not at all.

### Examples

- **Simple basic implementation**. For a basic implementation that sends new transactions over UDP, please refer to [this fork](https://github.com/ffakenz/hydra/tree/udp-sink).

- **Complex implementation**. For more advanced implementations using S3 and AWS Kinesis as event sources and sinks, visit the `doug_hydra_changes` branch [here](https://github.com/SundaeSwap-finance/hydra), particularly the [AWS Kinesis implementation](https://github.com/SundaeSwap-finance/hydra/blob/f27e51c001e7b64c3679eab4efd9f17f08db53fe/hydra-node/src/Hydra/Events/AWS/Kinesis.hs).

Currently, there is no CLI API to toggle which sources and sinks are utilized; this configuration must be manually implemented by the developers of the sources and sinks. Refer to the source and sink configuration example [here](https://github.com/SundaeSwap-finance/hydra/blob/4785bd86a03b92ba8fa8fb34c9d485a1e2f4f7d7/hydra-node/src/Hydra/Node/Run.hs#L97), where the event sinks and source are toggled through CLI options.

### Building event sinks and sources

- **Event sink construction**. Create an `EventSink e m` object, where `m` denotes the monad (such as IO) in which the event sink operates, and `e` represents the event type. The only field in the `EventSink` record corresponds to the monadic action taken upon a new event. See an example with AWS Kinesis [here](https://github.com/SundaeSwap-finance/hydra/blob/598b20fcee9669a196781f70e02e13779967e470/hydra-node/src/Hydra/Events/AWS/Kinesis.hs#L85).

- **Event source construction**. Construct an `EventSource e m` object, where the encapsulated monadic action produces a list of events `[e]`. An example loading events from AWS Kinesis is available [here](https://github.com/SundaeSwap-finance/hydra/blob/598b20fcee9669a196781f70e02e13779967e470/hydra-node/src/Hydra/Events/AWS/Kinesis.hs#L85). Consider implementing delays to manage the rate of event list construction, as the entire list is replayed at node startup, potentially overwhelming any API if not adequately throttled.
