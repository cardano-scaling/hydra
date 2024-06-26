---
sidebar_position: 4
---

# Extend the node with event source and sinks

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

## Offline mode

Hydra also supports an offline mode, which allows for disabling the layer 1 interface (that is, the underlying Cardano blockchain, which Hydra heads use to seed and withdraw funds to). This offline mode makes operation only influenced by the HTTP/WebSocket APIs, and the configured event sinks and sources. While separate from the event sinks and event source functionality, disabling layer 1 interactions allows for further customization, enabling use cases that would otherwise require running and configuring an entire layer 1 private devnet. See offline mode documentation [here](../configuration.md#offline-mode).
