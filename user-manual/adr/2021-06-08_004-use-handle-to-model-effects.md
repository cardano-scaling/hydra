---
slug: 4
title: | 
  4. Use Handle to model Effects
authors: []
tags: [Accepted]
---

## Status

Accepted

## Context

Given we are structuring Hydra node as a [reactive core](/adr/2) we need a way to ensure a strict separation of pure and impure (or effectful) code.

We want to be able to test those impure/effectful parts of the code. This requires a means for exchanging the actual implementation for e.g. the function to send messages over a network.

Also we want the ability to swap implementations not only for testing, but also be able
to accommodate different usage scenarios, e.g. use a different middleware
depending on peer configuration.

In Haskell there are various common _patterns_ to model effects:
  * [Tagless final encoding](http://okmij.org/ftp/tagless-final/index.html) also known as _MTL-style_ although using typeclasses to implement is [not necessary](https://www.foxhound.systems/blog/final-tagless/), whereby Effect(s) are expressed as typeclass(es) which are propagated as constraints
  * [Free monads](https://reasonablypolymorphic.com/blog/freer-monads/), or any variant thereof like Eff, freer, extensible-effects, whereby effect(s) are expressed as ADTs which are _interpreted_ in the context of an _Effect stack_
  * [Handle](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html) pattern also known as _record-of-functions_ whereby effects are grouped together in a datatype with a single record constructor

(These tradeoffs also appear in other functional languages like
[F#](https://medium.com/@dogwith1eye/prefer-records-of-functions-to-interfaces-d6413af4d2c3))

There is not one most favored solution though and we all have various
experiences with these techniques.

## Decision

Effectful components of the Hydra node (our code) will be defined using the _Handle pattern_.

There might be other techniques in use because of libraries used etc.

## Consequences

For example, the network component is defined as:
  ```hs
  newtype Network m = Network
    { broadcast :: MonadThrow m => HydraMessage -> m ()
    }
  ```
There might be multiple `createNetwork :: m (Network m)` functions
