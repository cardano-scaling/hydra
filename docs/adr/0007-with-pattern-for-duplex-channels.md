# 7. Use With-pattern for Duplex channels communication

Date: 2021-06-08

## Status

Discussed

## Context

* _With pattern_ or _bracket pattern_ is a functional programming idiom, a particular instance of _Continuation-Passing Style_, whereby one component that controls some resource that is consumed by another component of the system, is created via a function that takes as argument a function consuming the resource, instead of returning it.
* This pattern allows safer reclaiming of sytem-level resources when the "wrapped" action terminates, whether normally or unexpectedly

## Decision

* We use this pattern to provide interfaces to all _active components_ of the system, eg. components which execute concurrently with other components of the system and exchange messages with them
* A prototypical signature of such a component could be:
  ```
  withXXX ::
    forall m inmsg outmsg .
    (inmsg -> m ()) ->
    -- ^Callback for messages sent by the component
    ((outmsg -> m ()) -> m a) ->
    -- ^Continuation that takes a callback to send messages to the component
    m a
  ```

## Consequences

* Components can be layered one on top of another to provide additional  behavior provided exchanged messages are identical or can adapted.
* When possible, the component should be agnostic about the messages it consumes/produces. It can then be useful to define the consuming part of the component as a [Contravariant functor]() and the producing part as a (covariant) `Functor` as this makes it possible to use standard `map`/`contramap` operatoins to transform messages.
