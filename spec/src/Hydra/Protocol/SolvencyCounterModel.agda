-- Machine-checked counter-model of the pre-fix commit digest (typecheck-only; imported
-- by Main so it stays checked, not rendered and not extracted).
--
-- The solvency induction (Solvency.lagda.typ) shows the post-fix digest is sufficient;
-- this module shows the pre-fix digest was insufficient, as a positive artifact rather
-- than a failed proof attempt. It distills the increment acceptance to the two conjuncts
-- the deposit-binding attack exploited - "the claimed deposit's digest matches the signed
-- one" and "the head grows by the claimed deposit's value" - over a concrete miniature
-- (deposits are records of numbers, the digest projections are honest functions), and
-- checks:
--
--   * `attack`: under the pre-fix digest (datum content only), a look-alike deposit
--     carrying the same datum but less value is accepted under the signature the
--     parties gave for the real deposit, and
--   * `insolvent`: the resulting head value falls short of what the parties credited, and
--   * `fixed`: under the post-fix digest (datum content plus the deposit's transaction
--     id), the same claim is not even well-digested - the digests differ by identity.
--
-- The miniature is deliberately small: it is a counter-model of the digest SCHEME, not
-- of the full validity-bundle set (whose abstract postulates would each need concrete
-- instantiation to say more). Its value is regression documentation - the attack shape
-- stays written down as something the checker can rebuild - and a template for
-- counter-modeling future scheme changes before they ship.
module Hydra.Protocol.SolvencyCounterModel where

open import Agda.Builtin.Nat using (Nat; _+_)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Sigma using (Σ; _,_)
open import Data.Empty using (⊥)

record Deposit : Set where
  field
    txId      : Nat   -- the transaction that created the deposit (its identity)
    datumHash : Nat   -- hash of the datum's recorded commit set (its content)
    value     : Nat   -- what the deposit output actually holds

-- Pre-fix: the signed commit digest covers the datum content only.
digestOld : Deposit → Nat
digestOld d = Deposit.datumHash d

-- Post-fix: the digest also binds the deposit's transaction id.
digestNew : Deposit → Σ Nat (λ _ → Nat)
digestNew d = Deposit.datumHash d , Deposit.txId d

-- The real deposit the parties observed and signed for, and a look-alike carrying a
-- copied datum (deposit datums are unauthenticated) but a fraction of the value.
real : Deposit
real = record { txId = 1 ; datumHash = 7 ; value = 100 }

fake : Deposit
fake = record { txId = 2 ; datumHash = 7 ; value = 1 }

-- What the parties signed and credited on L2: the real deposit's digest and value.
signedDigest : Nat
signedDigest = digestOld real

credited : Nat
credited = Deposit.value real

-- The pre-fix acceptance shape: the claimed deposit's digest matches the signed one,
-- and the head grows by the claimed deposit's value (both hold of the real increment
-- checks; everything else the validator looks at is satisfied by an otherwise honest
-- transaction).
record AcceptsOld (claimed : Deposit) (headIn headOut : Nat) : Set where
  field
    digestOK : digestOld claimed ≡ signedDigest
    valueOK  : headOut ≡ headIn + Deposit.value claimed

-- The attack, accepted: claiming the look-alike under the real deposit's signature.
attack : AcceptsOld fake 0 1
attack = record { digestOK = refl ; valueOK = refl }

-- The head is insolvent against what the parties credited: it absorbed 1, not 100.
insolvent : 1 ≡ 0 + credited → ⊥
insolvent ()

-- Post-fix, the look-alike's digest differs from the real deposit's by identity, so
-- the same claim cannot match the signed digest at all.
fixed : digestNew fake ≡ digestNew real → ⊥
fixed ()
