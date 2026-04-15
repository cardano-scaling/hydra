# Event sourcing

The `hydra-node` is an event sourced application. This means that the main logic is processing _inputs_ (also called commands) and produces _events_. These events are saved and loaded to persist application state across restarts. Also, most events are transformed to _outputs_ and can be observed on the `hydra-node` API.

On application startup, the [`hydrate`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Node.html#v:hydrate) function is called to load all events using a given [`EventSource`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Events.html#t:EventSource) and while doing so, re-emits those events to all provided [`EventSink`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Events.html#t:EventSink) instances. The resulting [`HydraNode`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Node.html#t:HydraNode) will then enter the main loop of `hydra-node` and process inputs into state changes and effects via function [`stepHydraNode`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Node.html#v:stepHydraNode). All state changes of a Hydra node are based on [`StateEvent`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Events.html#t:StateEvent) values and consequently get emitted to all `eventSinks` of the `HydraNode` handle. Also, the `eventSource` of the same may be used later to to load events on-demand, for example to produce a history of server outputs.

## Default event source and sinks

The default event source and sink used by the `hydra-node` is `SQLiteBased`, which persists events in a SQLite database file named `hydra.db`. This file is located in the `hydra-node` persistence directory, which is specified by the `--persistence-dir` command line option.

If a legacy `state` file from a previous file-based installation is found in the persistence directory on startup, its events are automatically migrated into `hydra.db` and the original file is renamed to `state.migrated`.

As explained in the consequences of [ADR29](https://hydra.family/head-protocol/adr/29), the API server of the `hydra-node` is also an event sink, which means that all events are sent to the API server and may be further submitted as [`ServerOutput`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-API-ServerOutput.html#t:ServerOutput) to clients of the API server. See [`mkTimedServerOutputFromStateEvent`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-API-Server.html#v:mkTimedServerOutputFromStateEvent) for which events are mapped to server outputs.

### SQLite database

#### Schema

The database contains a single `events` table:

| Column       | Type    | Description                              |
|-------------|---------|------------------------------------------|
| `event_id`  | INTEGER | Primary key, matches the in-memory event id |
| `event_data` | BLOB   | JSON-encoded event payload               |

The following connection pragmas are set on every open:

- `journal_mode=WAL` — write-ahead logging for concurrent reads during writes
- `synchronous=NORMAL` — skips per-write fsyncs (safe because the L1 chain is the source of truth)
- `busy_timeout=5000` — wait up to 5 s for locks instead of failing immediately
- `cache_size=-65536` — 64 MB page cache
- `temp_store=MEMORY` — temporary tables and indexes in memory

#### Async write-behind

To keep persistence off the hot path, writes use an async write-behind strategy. `putEvent` encodes events eagerly to strict `ByteString` and enqueues them into a bounded `TBQueue`. A background writer thread drains the queue and batch-inserts rows using a single transaction, amortising WAL frame writes across multiple events.

The last-seen event id is updated atomically at enqueue time (not write time), so de-duplication tracking remains correct even though the physical write is deferred. Reads via `sourceEvents` auto-flush the write queue before reading, so callers always see all enqueued events without manual synchronisation.

#### Schema versioning

The database schema is versioned using SQLite's built-in `PRAGMA user_version`. On startup, `initSchema` reads the current version and applies any pending migration steps incrementally up to `nextVersion`. Each step is defined as a case in `migrateStep`:

```haskell
migrateStep conn = \case
  0 -> -- create the events table
  1 -> -- (future) e.g. add an index or new column
```

A fresh database starts at version 0 (SQLite default). After all migrations run, `user_version` is set to `nextVersion`. If the database has a version higher than `nextVersion` (e.g. from a newer release), the node refuses to start to prevent silent data corruption on downgrade.

To add a new migration:

1. Bump `nextVersion` in `Hydra.Events.SQLiteBased`
2. Add a new case to `migrateStep` for the previous version number
3. Add tests in `SQLiteBasedSpec` to verify the migration

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

Rotation can be enabled via the optional `--persistence-rotate-after` command-line argument, which specifies the number of events after which rotation should occur. When rotation triggers, all events older than the checkpoint are deleted from `hydra.db` and replaced with a single checkpoint event capturing the current aggregated state.
> For example, with `--persistence-rotate-after 100`, a rotation occurs after every 100 new events. The checkpoint event captures the full node state at that point, so on the next restart the node only needs to replay events since the last checkpoint.

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
