# Event sourcing

The `hydra-node` is an event sourced application. This means that the main logic is processing _inputs_ (also called commands) and produces _events_. These events are saved and loaded to persist application state across restarts. Also, most events are transformed to _outputs_ and can be observed on the `hydra-node` API.

On application startup, the [`hydrate`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Node.html#v:hydrate) function is called to load all events using a given [`EventSource`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Events.html#t:EventSource) and while doing so, re-emits those events to all provided [`EventSink`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Events.html#t:EventSink) instances. The resulting [`HydraNode`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Node.html#t:HydraNode) will then enter the main loop of `hydra-node` and process inputs into state changes and effects via function [`stepHydraNode`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Node.html#v:stepHydraNode). All state changes of a Hydra node are based on [`StateEvent`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Events.html#t:StateEvent) values and consequently get emitted to all `eventSinks` of the `HydraNode` handle. Also, the `eventSource` of the same may be used later to to load events on-demand, for example to produce a history of server outputs.

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

### Event Log Rotation

Long-living heads may produce a large number of persisted events, which can impact the restart time of the hydra-node as it needs to read in all the previous to recreate its state.

Event log rotation was introduced to improve recovery times by reducing the number of events that need to be replayed on startup. This is achieved by periodically replacing the current event log with a new one that starts from a checkpoint event, which captures the latest aggregated head state.

Only rotated log files are saved with an incrementing `logId` suffix in their names, while the main `state` log file remains unchanged to preserve backward compatibility. This `logId` suffix corresponds to the ID of the last event included in that file.
Rotation can be enabled via the optional `--persistence-rotate-after` command-line argument, which specifies the number of events after which rotation should occur.
> For example, with `--persistence-rotate-after 100`, you’ll get rotated files named: state-100, state-200, state-300, and so on, each containing 101 events. This is because event IDs start at 0, so state-100 includes 101 state changed events (0–100) without a checkpoint. Subsequent rotated files include a checkpoint plus 100 new state changed events.

Note that a checkpoint event id matches the last persisted event id from the previous rotated log file, preserving the sequential order of event ids across logs.
This also makes it easier to identify which rotated log file was used to compute the checkpoint, as its event id matches the file name suffix.

Depending on the rotation configuration used, the current `state` file may already contain more events than the specified threshold, causing a rotation to occur immediately on startup before any new inputs are processed.

Upon rotation, a server output is produced to notify external agents when a checkpoint occurs, allowing them to perform archival or cleanup actions without interrupting the Hydra Head.

The appropriate value for `--persistence-rotate-after` depends on your specific use case and the expected transaction volume.

> As a rough guideline, in a simple scenario (running a single party on devnet that repeatedly re-spends the same committed UTxO) we observed that setting `--persistence-rotate-after 10000` results in rotated log files of about 8 MB every 3 minutes.
>
> Keep in mind that the size and frequency of rotated files will vary depending on several factors:
>  * Transaction sizes: Larger transactions result in larger event payloads.
>  * Number of party members: More parties increase the number of L2 protocol messages per snapshot, generating more events.
>  * Ledger UTxO size: A higher number of UTxOs increases the size of certain events like snapshots.
>  * Transaction throughput (TPS): Higher TPS leads to more events being produced over time.
