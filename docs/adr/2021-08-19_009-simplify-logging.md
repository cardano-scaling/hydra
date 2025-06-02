---
slug: 9
title: | 
  9. Simplify Logging
authors: []
tags: [Accepted]
---

## Status

Accepted

## Context

* Logs are critical to provide _observability_ to Hydra nodes' operators
* Providing the needed components and tools to be able to configure logging and monitoring to each operator's liking should not be the responibility of the Hydra node, and requires complex machinery that will need to be maintained and evolved
* When a problem occurs in production, if the process is not verbose enough it can be very hard to analyse the problem
  * Enabling dynamic changes of verbosity in logs is both complex to implement and comes too late
  * Deciding in the code on what's the right "severity" for a log entry leads to dropping important information on _how_ some error occurred

## Decision

_Therefore_

Hydra node provides a very simplified logging mechanism whereby:
* All logs are emitted as JSON-encoded structures providing some metadata (timestamp, threadId) around well-defined data
* Each _log entry_ is written to the `hydra-node` process' _stdout_ port, one line per entry

**Note**: Proper redaction of sensitive information contained in log entries should still be handled in the code.

## Consequences

* It is the responsibility of the node operator to filter severity or otherwise post-process the logs
