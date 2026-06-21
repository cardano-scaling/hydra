```
module Hydra.Protocol.Preliminaries where

open import Hydra.Protocol.Prelude
```

#import "/template.typ": *
#import "/macros.typ": *
#import "/diagrams.typ": utxo-graph

= Preliminaries <sec:prel>

This section introduces notation and other preliminaries used in the remainder
of the specification.

== Notation

The specification uses set-notation based approach while also inspired
by~@eutxo-2~and~@eutxo. Values $a$ are in a set $a in cal(A)$,
also indicated as being of some type $a : cal(A)$, and multidimensional values are
tuples drawn from a $times$ product of multiple sets, e.g.
$(a, b) in (cal(A) times cal(B))$. An empty set is indicated by
$emptyset$ and sets may be enumerated using ${a_1 dots.h a_n}$ notation. The $=$ operator means
equality and $<-$ is explicit assignment of a variable or value to one
or more variables. Projection is used to access the elements of a tuple, e.g.
$(a, b)^(arrow.b 1) = a$. Functions are morphisms mapping from one set to another
$x : cal(A) -> f(x) : cal(B)$, where function
application of a function $f$ to an argument $x$ is written as $f(x)$.

#v(0.5em)
Furthermore, given a set $cal(A)$, let

- $cal(A)^? = cal(A) union lozenge$ denotes an option: a value from $cal(A)$ or no value at all indicated by $bot$,
- $cal(A)^n$ be the set of all n-sized sequences over $cal(A)$,
- $cal(A)^! = union.big_(i = 1)^(n in tyNatural) cal(A)^i$ be the set of non-empty sequences over $cal(A)$, and
- $cal(A)^* = union.big_(i = 0)^(n in tyNatural) cal(A)^i$ be
  the set of all sequences over $cal(A)$.

#v(0.5em)
With this, we further define:

- $tyBool = {mfalse, mtrue}$ are boolean values
- $tyNatural$ are natural numbers ${0, 1, 2, dots.h}$
- $tyInteger$ are integer numbers ${dots.h, -2, -1, 0, 1, 2, dots.h}$
- $tyBytes = union.big_(n = 0)^(infinity) {0, 1}^(8 n)$ denotes a arbitrary
  string of bytes
- $concat : tyBytes^* -> tyBytes$ is concatenating bytes, we also use operator $plus.o.big$ for this
- $hash : x -> tyBytes$ denotes a collision-resistant
  hashing function and $x^(\#)$ indicates the hash of $x$
- $bytes : x -> tyBytes$ denotes an invertible serialisation function
  mapping arbitrary data to bytes
- $a || b = concat(bytes(a), bytes(b))$ is an operator which concatenates the $bytes(b)$ to the $bytes(a)$
- Lists of values $l in cal(A)^*$ are written as
  $l = [x_1, dots.h, x_n]$. Empty lists are denoted by $[]$, the $i$th
  element $x_i$ is also written $l[i]$ and the length of the list is
  $|l| = n$. An underscore is also used to indicate a list of values
  $underline(x) = l$. Projection on lists are mapped to their elements,
  i.e.
  $underline(x)^(arrow.b 1) = [x_1^(arrow.b 1), dots.h, x_n^(arrow.b 1)]$.
- $sortOn : i -> cal(A)^* -> cal(A)^*$ does sort a list of
  values on the $i$th projection.
- $tyData$ is a universal data type of nested sums and products built up
  recursively from the base types of $tyInteger$ and $tyBytes$.

The concatenation operator is defined in Agda directly in terms of $bytes$ and
$concat$; Agda checks that the operands are serialised before being concatenated
(i.e. that the operation is well-typed):

```agda
infixl 6 _‖_
_‖_ : ∀ {A B : Set} → A → B → ℍ
a ‖ b = concat (bytes a ∷ bytes b ∷ [])
```

== Public key multi-signature scheme <sec:multisig>
// TODO: move/merge with protocol setup and make concrete
A multisignature scheme is a set of algorithms where

- $msSetup$ generates public parameters $msParams$, such that
- $(msVK, msSK) <- msKeyGen(msParams)$ can be used to generate fresh
  key pairs,
- $msSig <- msSign(msParams, msSK, msMsg)$ signs a message $msMsg$
  using key $msSK$,
- $msCVK <- msCombVK(msParams, msVKL)$ aggregates a list of
  verification keys $msVKL$ into a single, aggregate key $msCVK$,
- $msCSig <- msComb(msParams, msMsg, msVKL, msSigL)$ aggregates a
  list of signatures $msSigL$ about message $m$ into a single, aggregate
  signature~$msCSig$.
- $msVfy(msParams, msCVK, msMsg, msCSig) in tyBool$ verifies an aggregate
  signature $msCSig$ of message $msMsg$ under an aggregate verification
  key $msCVK$.

The security definition of a multisignature scheme
from~@itakura1983public@CCS:MicOhtRey01 guarantees that, if $msCVK$ is
produced from a tuple of verification keys $msVKL$ via $msCombVK$, then no
aggregate signature $msCSig$ can pass verification
$msVfy(msCVK, msMsg, msCSig)$ unless all honest parties holding keys in
$msVKL$ signed $m$.

Note that in the following, we make the parameter~$msParams$ implicit and leave
out the $italic("ver")$ suffix for verification key such that $k = k^("ver")$ for better
readability.

As an Agda interface, a multisignature scheme is a record of the abstract key,
signature and message types together with these algorithms (the public
parameters $msParams$ are kept implicit, as above):

```agda
record MultiSignatureScheme : Set₁ where
  field
    VK SK Sig Msg : Set
    keyGen    : VK × SK
    sign      : SK → Msg → Sig
    combineVK : List VK → VK              -- aggregate verification key k̃
    combine   : Msg → List VK → List Sig → Sig  -- aggregate signature σ̃
    verify    : VK → Msg → Sig → Bool
```

The security definition (above) is a property over this interface: a `verify`
against an aggregate key only succeeds if all honest key holders signed.

== Extended UTxO <sec:eutxo>
The Hydra Head protocol is specified to work on the so-called Extended UTxO (EUTxO) ledgers
like Cardano.

The basis for EUTxO is Bitcoin's UTxO ledger
model~@formal-model-of-bitcoin-transactions@Zahnentferner18-UTxO.
Intuitively, it arranges transactions in a directed acyclic graph, such as the
one in @fig:utxo-graph, where boxes represent transactions with
(red) inputs to the left and (black) outputs to the right. A dangling
(unconnected) output is an _unspent transaction output (UTxO)_ - there
are two UTxOs in the figure.

#figure(utxo-graph, caption: [Example of a plain UTxO graph]) <fig:utxo-graph>

The following paragraphs will give definitions of the UTxO model and it's
extension to support scripting (EUTxO) suitable for this Hydra Head protocol
specification. For a more detailed introduction to the EUTxO ledger model,
see~@eutxo,~@eutxo-2~and~@utxo-ma.

=== Values

#definition(name: [Values])[
  Values are sets that keep track of how many units of which tokens of which
  currency are available. Given a finitely supported function $|->$, that
  maps keys to monoids, a value is the set of such mappings over currencies
  (minting policy identifiers), over a mapping of token names $t$ to
  quantities $q$:
  $
    val in tyValue = (c : tyBytes |-> (t : tyBytes |-> q : tyInteger))
  $
  where addition of values is defined as $+$ and $diameter$ is the empty value.
]

For example, the value ${c_1 |-> {t_1 |-> 1, t_2 |-> 1}}$
contains tokens $t_1$ and $t_2$ of currency $c_1$ and addition merges
currencies and token names naturally:
$
  & {c_1 |-> {t_1 |-> 1, t_2 |-> 1}} \
  + #h(0.5em) & {c_1 |-> {t_2 |-> 1, t_3 |-> 1}, c_2 |-> { t_1 |-> 2}} \
  = #h(0.5em) & {c_1 |-> {t_1 |-> 1, t_2 |-> 2, t_3 |-> 1}, c_2 |-> { t_1 |-> 2}} #h(0.3em) .
$

While the above definition should be sufficient for the purpose of this
specification, a full definition for finitely supported functions and values as
used here can be found in~@utxo-ma. To further improve readability, we
define the following shorthands:

- ${t_1, dots.h, t_n} :: c$ for a set positive single quantity assets
  ${c |-> {t_1 |-> 1, dots.h, t_n |-> 1}}$,
- ${t_1, dots.h, t_n}^(-1) :: c$ for a set of negative single quantity assets
  ${c |-> {t_1 |-> -1, dots.h, t_n |-> -1}}$,
- ${c |-> t |-> q}$ for the value entry ${c |-> {t |-> q}}$,
- ${c |-> dot.c |-> q }$ for any asset with currency $c$ and
  quantity $q$ irrespective of token name.

=== Scripts

Validator scripts are called _phase-2_ scripts in the Cardano Ledger
specification (see~@ledger-alonzo-spec for a formal treatment of these). Scripts
are used for multiple purposes, but most often (and sufficient for this
specification) as a _spending_ or _minting_ policy script.

#definition(name: [Minting Policy Script])[
  A script $mu in cal(M)$ governing whether a value can be minted (or
  burned), is a pure function with type
  $
    mu in cal(M) = (rho : tyData) -> (txContext : tyContext) -> tyBool,
  $
  where $rho in tyData$ is the redeemer provided as part of the transaction
  being validated and $txContext in tyContext$ is the validation
  context.
]

#definition(name: [Spending Validator Script])[
  A validator script $nu in cal(V)$ governing whether an output can be
  spent, is a pure function with type
  $
    nu in cal(V) = (delta : tyData) -> (rho : tyData) -> (txContext : tyContext) -> tyBool,
  $
  where $delta in tyData$ is the datum available at the output to be spent,
  $rho in tyData$ is the redeemer data provided as part of the transaction
  being validated, and $txContext in tyContext$ is the validation
  context.
]

=== Transactions
#todo[actual transactions $cal(T)$ are not defined]

We define EUTxO inputs, outputs and transactions as they are available to
scripts and just enough to specify the behavior of the Hydra validator scripts.
For example outputs addresses and datums are much more complicated in the full
ledger model~@eutxo-2@ledger-shelley-spec.

#definition(name: [Outputs])[
  An output $o in txOutputs$ stores some value $val in tyValue$ at some address,
  defined by the hash of a validator script $nu^(\#) in tyBytes = hash(nu in cal(V))$,
  and may store (reference) some data $delta in tyData$:
  $
    o in txOutputs = (val : tyValue times nu^(\#) : tyBytes times delta : tyData)
  $
]

#definition(name: [Output references])[
  An output reference $txOutRef in tyOutRef$ points to an output of a
  transaction, using a transaction id (that is, a hash of the transaction body)
  and the output index within that transaction.
  $
    txOutRef in tyOutRef = (tyBytes times bb(N))
  $
]

#definition(name: [Inputs])[
  A transaction input $i in txInputs$ is an output reference
  $txOutRef in tyOutRef$ with a corresponding redeemer $rho in tyData$:
  $
    i in txInputs = (txOutRef : tyOutRef times rho : tyData)
  $
]

#definition(name: [Validation Context])[
  A validation context $txContext in tyContext$ is a view on the transaction
  to be validated:
  $
    txContext in tyContext = (tyInputs times tyOutputs times tyValue times cal(S)^(<->) times cal(K))
  $
  where $txInputs in tyInputs$ is a (ledger-level) set of _resolved_ inputs --- each carries the
  spent output (its value, address and datum), as in the eUTxO ledger --- modelled in Agda as a
  *list* so the value locked at a given script can be summed;
  $txOutputs in tyOutputs$ is a *list* of outputs,
  $txMint in tyValue$ is the minted (or burned) value,
  $(txValidityMin, txValidityMax) in tyValidity$ are the lower and upper
  validity bounds where $txValidityMin <= txValidityMax$, and
  $txKeys in cal(K)$ is the set of verification keys which signed the
  transaction. The context additionally exposes the hash of the validator script currently being
  run ($sans("ownHash")$), against which a state-machine validator identifies its own continuing
  input and output (and hence the head value in/out).
  // TODO: tyValidity undefined, define time, periods and intervals?
]

As a first step of the machine-checked formalisation, these definitions are
captured directly in Agda (with the value, script and key types kept abstract for
now):

```agda
record Output : Set where
  field
    value   : Value
    address : ℍ            -- the validator script hash ν#
    datum   : Data

record OutputRef : Set where
  field
    txId  : ℍ
    index : ℕ

record Input : Set where
  field
    outputRef : OutputRef
    resolved  : Output      -- the spent output (value/address/datum), as in Plutus 'TxInInfo'
    redeemer  : Data

record ValidityInterval : Set where
  field
    lo hi : ℕ              -- lower/upper validity bounds
    lo≤hi : lo ≤ hi        -- the spec's constraint, now enforced by the type

record Context : Set where
  field
    ownHash  : ℍ            -- hash of the validator script being run (e.g. νHead)
    depHash  : ℍ            -- hash of the νDeposit script (so deposit inputs can be summed, cf. §5.4)
    inputs   : List Input   -- the transaction's resolved inputs (a set at the ledger level;
                            -- modelled as a list so per-script value can be summed, cf. Plutus)
    outputs  : List Output  -- a list of outputs
    mint     : Value
    validity : ValidityInterval
    keys     : ℙ VKey       -- verification keys that signed the transaction
```

Informally, scripts are evaluated by the ledger when it applies a transaction to
its current state to yield a new ledger state (besides checking the transaction
integrity, signatures and ledger rules). Each validator script referenced by
an output is passed its arguments drawn from the output it locks and the
transaction context it is executed in. The transaction is valid if and only if
all scripts validate, i.e. $mu(rho, txContext) = mtrue$ and
$nu(delta, rho, txContext) = mtrue$.

=== State machines and graphical notation

State machines in the EUTxO ledger model are commonly described using the
_constraint emitting machine (CEM)_ formalism~@eutxo, e.g.~the
original paper describes the Hydra Head protocol using this
notation~@hydrahead20. Although inspired by CEMs, this specification uses
a more direct representation of individual transactions to simplify description
of non-state-machine transactions and help translation to concrete
implementations on Cardano. The structure of the state machine is enforced
on-chain through _scripts_ which run as part of the ledger's validation of
a transaction (see @sec:eutxo). For each protocol transaction, the
specification defines the structure of the transaction and enumerates the
transaction constraints enforced by the scripts ($cemTxCon$ in the CEM
formalism).

// TODO: Create example with generic labels and point out that state
// input/outputs do represent a transition in the statemachine from s' to s' etc.
#todo[Add an example graph with a legend]

== KZG Accumulators <sec:accumulators>

An accumulator scheme over a universe $cal(U)$ is a set of algorithms where

- $accSetup$ generates public parameters (the common reference string),
- $eta <- accUTxO(U)$ constructs an accumulator commitment from a UTxO set $U subset.eq cal(U)$,
- $eta' <- accCombine(eta_1, eta_2)$ combines two accumulator commitments into one,
- $pi <- accWitness(eta, S, U backslash S)$ produces a membership witness $pi$ proving that $S subset.eq U$,
- $accVerify(eta, S, pi) in tyBool$ verifies a membership witness for subset $S$,
- $eta' <- accExclude(eta, S)$ removes subset $S$ from the accumulator, yielding the updated commitment $eta'$, which itself serves as the exclusion witness, and
- $accVerifyExclude(eta, S, eta') in tyBool$ verifies that $eta'$ is the correct accumulator after removing $S$ from $eta$.

The security property guarantees that a valid membership witness can only be produced for sets genuinely committed under $eta$, and a valid exclusion witness can only be produced when the subset was genuinely removed.

As an Agda interface, the scheme is a record over the abstract universe $cal(U)$
(`Item`), commitments $eta$ (`Commitment`) and witnesses $pi$ (`Witness`):

```agda
record Accumulator : Set₁ where
  field
    Item Commitment Witness : Set
    commit        : ℙ Item → Commitment                       -- η ← accUTxO(U)
    combine       : Commitment → Commitment → Commitment       -- accCombine
    witness       : Commitment → ℙ Item → ℙ Item → Witness     -- accWitness(η, S, U∖S)
    verify        : Commitment → ℙ Item → Witness → Bool       -- accVerify
    exclude       : Commitment → ℙ Item → Commitment           -- accExclude
    verifyExclude : Commitment → ℙ Item → Commitment → Bool    -- accVerifyExclude
```

The implementation uses KZG polynomial commitments~@KZG10 over the BLS12-381 elliptic curve.
