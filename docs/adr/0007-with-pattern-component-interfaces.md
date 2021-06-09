# 7. Use with-pattern based component interfaces

Date: 2021-06-08

## Status

Proposed

TBD:
* Naming of `Callback` and `Component`
* Demonstrate the `Covariant` consequence
* Provide context about tying the knot

## Context

The _with pattern_ or _bracket pattern_ is a functional programming idiom, a
particular instance of _Continuation-Passing Style_, whereby one component that
controls some resource that is consumed by another component of the system, is
created via a function that takes as argument a function consuming the resource,
instead of returning it. This pattern allows safe reclaiming of resources when
the "wrapped" action terminates, whether normally or unexpectedly.

TODO "Tying the knot"

## Decision

We use this pattern to provide interfaces to all _active components_, which
exchange messages with other components of the system. A prototypical signature
of such a component could be:

  ```hs
  type Component m = outmsg -> m ()
  type Callback m = inmsg -> m ()

  withXXX :: Callback m -> (Component m -> m a) -> m a
  ```

Note that `withXXX` can also allocate resources in order to provide `Component`
or use the `Callback`, e.g. fork threads which invoke `Callback`, but also make
sure they are cleaned up.

## Consequences

Components can be layered on top of another to provide additional behavior given the same interface. This also similar to "decorating" in the object-orientation world.

If the `Component` is agnostic about the messages it consumes/produces, it can be defined as a [`Contravariant` functor](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Functor-Contravariant.html) and the `Callback` part as a (covariant) `Functor`. This makes it possible to use `map` and `contramap` operations to transform messages.
