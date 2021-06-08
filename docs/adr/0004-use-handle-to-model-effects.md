# 4. Use Handle to model Effects

Date: 2021-06-08

## Status

Accepted

## Context

* Given we are structuring Hydra node as a [reactive core](0002-reactive-core) we need a way to ensure a strict separation of pure and impure (or effectful) code
* Various _patterns_ have been proposed in Haskell to model effects:
  * [Tagless final encoding](http://okmij.org/ftp/tagless-final/index.html) also known as _MTL-style_ although using typeclasses to implement is [not necessary](https://www.foxhound.systems/blog/final-tagless/), whereby Effect(s) are expressed as typeclass(es) which are propagated as constraints
  * [Free monads](https://reasonablypolymorphic.com/blog/freer-monads/), or any variant thereof like Eff, freer, extensible-effects, whereby effect(s) are expressed as ADTs which are _interpreted_ in the context of an _Effect stack_
  * [Handle](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html) pattern also known as _record-of-functions_ whereby effects are grouped together in a datatype with a single record constructor
* All options considered have both advocates and detractors and there is no widely accepted pattern
* We did a couple experiments to explore various options

## Decision

* Effectful components of the Hydra node will be defined using  the _Handle pattern_

## Consequences

### Effects

* For example, the on-chain component is defined as:
 ```
  newtype OnChain m = OnChain
  { postTx :: MonadThrow m => OnChainTx -> m ()
  }
  ```
* Instantiation to concrete IO is pushed at the outermost layer, eg. in the `Main` or tests

## Additional comments

* These tradeoffs also appear in other functional languages like [F#](https://medium.com/@dogwith1eye/prefer-records-of-functions-to-interfaces-d6413af4d2c3)
