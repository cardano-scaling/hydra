---
slug: 25
title: |
  25. Stateless API for consuming persistence events, and ingesting transactions into persistence
authors: []
tags: []
---

## Status

N/A

## Context

- Current client API is a stateful WebSocket connection. Client API has its own incrementally persisted state that must be maintained, and includes a monotonically increasing `Seq` number, but this is based off the events dealing with the client, not the host hydra-node.

- Client must be flexible and ready to handle many different events

- There is no way to simply hand off transactions to the hyrda-node currently, a full connection must be initiated before observed transactions can be applied, and bulky JSON objects can slow down "bulk" operations

- Many applications, like SundaeSwap's Gummiworm Protocol, do not need need the full general API, and benefit from any performance improvement

# Decision

- Persistence event log will have a local observed event order written into each event, as a monotonic integer (global event ID). This should be made explicitly different from any type of `Seq` monotonic value. It will always be delivered in stateless client mode, to resume connections.

- (Up to) full duplex Websocket server, on a different port, to listen for submitting transactions statelessly to the hydra node. Doesn't have to be limited to transactions. Can handle CBOR. Configuration is done on each connection entirely through query strings or HTTP headers and upgrade, including a starting 
    - No handshake needed if write-only enabled in query string, just open connection and submit info and optionally listen for a submission-level ACK (only contains new global event ID)
    - Basic subscription filtering, you can subscribe to different "types" of events, including multiple at the same time
        - Only type relevant to Gummiworm is transaction recieved / approved as valid / rejected
    - Client may not change any parameters of the subscription, just resume with a new connection
    - No guarantee of acking in case a connection dies, but this is not an issue as the client can simply reinitiate a connection from the last point it remembered, and stream new events, and resubmit anything
    - Global event order of events the stateless client sees will be always increasing, but may not increase by the same amount (monotonic, not monotonically increasing). Seq count will do the latter, but will only be consistent within a single session
    - Transactions can be dropped off as CBOR, if configured in the query string. The entire protocol in this case can just be an indefinite encoded stream of CBOR transactions, optionally with json object wrapping them to support heart beats or any other necessities

- Websocket server replies can be configured to be written (via same incremental persistence) to a file, forming an incremental view of, say, accepted transactions
    - Client connection that does this can "mute" itself
    - When connection dies, incremental view is no longer updated


## Other persistence improvements Gummiworm benefits from, but are not vital

- Can also do the same with a file that's read in read-only line buffered mode, which can be used to easily pipe in transactions
    - Important for debugging

- Persistence event log can be combined with cumulative state (similar to non-incremental state before), simply by tying it to global event ID from the persistence event log, and making sure the file write is atomic.
    - Functions soley as a cache for persistence log, so it can be written periodically, and doesn't need to block

- Semi-index (hw-json-simd) can efficiently seek persistence log to replay from a particular global event ID, such as the last one recorded above

- `streaming` library can allow for restoring from persistence event log in constant space, important for L2s like gummiworm with very large sessions

- zstd compression can be used to further accelerate persistence log replay, save space on disk, lower the amount of time it takes to flush to disk before processing next event
    - Adaptive compression mode in zstd CLI designed for compressing logs, if it's available via haskell bindings, itd be ideal
    - Compression timing attacks might be possible, this can be mitigated somewhat by using a dictionary trained entirely on redacted logs, to minimize the effect of 

- Event log append and loadAll should be configurable via separate flags that default to the same (this way append can instead be stubbed out to point)
    - Not necessary for gummiworm validator node, but can be useful for, say, `tee`ing and backing up persisted persistence event log in real time

# Effects

- Two APIs, documentation necessary to help applications choose between the general API and stateless API
    - Long term, it may be good to unify as much as possible
