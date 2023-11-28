# Preliminaries {#sec:prel}

<!-- TODO: chapter / page breaks -->

This section introduces notation and other preliminaries used in the
remainder of the specification.

## Notation

The specification uses set-notation based approach while also inspired
by [@eutxo-2] and [@eutxo]. Values $a$ are in a set $a \in \mathcal{A}$,
also indicated as being of some type $a : \mathcal{A}$, and
multidimensional values are tuples drawn from a $\times$ product of
multiple sets, e.g. $(a,b) \in (\mathcal{A} \times \mathcal{B})$. An
empty set is indicated by $\emptyset$ and sets may be enumerated using
$\{a_1 \dots a_n\}$ notation. The $=$ operator means equality and
$\gets$ is explicit assignment of a variable or value to one or more
variables. Projection is used to access the elements of a tuple, e.g.
${(a,b)}^{\downarrow1} = a$. Functions are morphisms mapping from one
set to another $x : \mathcal{A} \to f(x) : \mathcal{B}$, where function
application of a function $f$ to an argument $x$ is written as $f(x)$.

Furthermore, given a set $\mathcal{A}$, let

-   $\mathcal{A}^? = \mathcal{A} \cup \Diamond$ denotes an option: a
    value from $\mathcal{A}$ or no value at all,

-   $\mathcal{A}^n$ be the set of all n-sized sequences over
    $\mathcal{A}$,

-   $\mathcal{A}^! = \bigcup_{i=1}^{n \in \tyNatural} \mathcal{A}^{i}$
    be the set of non-empty sequences over $\mathcal{A}$, and

-   $\mathcal{A}^* = \bigcup_{i=0}^{n \in \tyNatural} \mathcal{A}^{i}$
    be the set of all sequences over $\mathcal{A}$.

With this, we further define:

-   $\tyBool = \{\false, \true\}$ are boolean values

-   $\tyNatural$ are natural numbers $\{0, 1, 2, \ldots\}$

-   $\tyInteger$ are integer numbers
    $\{\ldots, −2, −1, 0, 1, 2, \ldots\}$

-   $\tyBytes = \bigcup_{n=0}^{\inf}{\{0,1\}}^{8n}$ denotes a arbitrary
    string of bytes

-   $\concat : \tyBytes^* \to \tyBytes$ is concatenating bytes, we also
    use operator $\bigoplus$ for this

-   $\hash : x \to \tyBytes$ denotes a collision-resistant hashing
    function and $x^{\#}$ indicates the hash of $x$

-   $\bytes : x \to \tyBytes$ denotes an invertible serialisation
    function mapping arbitrary data to bytes

-   $a || b = \concat(\bytes(a), \bytes(b))$ is an operator which
    concatenates the $\bytes(b)$ to the $\bytes(a)$

-   Lists of values $l \in \mathcal{A}^{*}$ are written as
    $l = [x_{1}, \ldots, x_{n}]$. Empty lists are denoted by $[]$, the
    $i$th element $x_{i}$ is also written $l[i]$ and the length of the
    list is $|l| = n$. An underscore is also used to indicate a list of
    values $\underline{x} = l$. Projection on lists are mapped to their
    elements, i.e.
    $\underline{x}^{\downarrow1} = [x_{1}^{\downarrow1}, \dots, x_{n}^{\downarrow1}]$.

-   $\sortOn : i \to \mathcal{A}^{*} \to \mathcal{A}^{*}$ does sort a
    list of values on the $i$th projection.

-   $\tyData$ is a universal data type of nested sums and products built
    up recursively from the base types of $\tyInteger$ and $\tyBytes$.

## Public key multi-signature scheme {#sec:multisig}

A multisignature scheme is a set of algorithms where

-   $\msSetup$ generates public parameters $\msParams$, such that

-   $(\msVK,\msSK) \gets \msKeyGen(\msParams)$ can be used to generate
    fresh key pairs,

-   $\msSig \gets \msSign(\msParams,\msSK,\msMsg)$ signs a message
    $\msMsg$ using key $\msSK$,

-   $\msCVK \gets \msCombVK(\msParams,\msVKL)$ aggregates a list of
    verification keys $\msVKL$ into a single, aggregate key $\msCVK$,

-   $\msCSig \gets \msComb(\msParams,\msMsg,\msVKL,\msSigL)$ aggregates
    a list of signatures $\msSigL$ about message $m$ into a single,
    aggregate signature $\msCSig$.

-   $\msVfy(\msParams,\msCVK,\msMsg,\msCSig) \in \tyBool$ verifies an
    aggregate signature $\msCSig$ of message $\msMsg$ under an aggregate
    verification key $\msCVK$.

The security definition of a multisignature scheme
from [@itakura1983public; @CCS:MicOhtRey01] guarantees that, if $\msCVK$
is produced from a tuple of verification keys $\msVKL$ via $\msCombVK$,
then no aggregate signature $\msCSig$ can pass verification
$\msVfy(\msCVK,\msMsg,\msCSig)$ unless all honest parties holding keys
in $\msVKL$ signed $m$.

Note that in the following, we make the parameter $\msParams$ implicit
and leave out the $ver$ suffix for verification key such that
$k = k^{ver}$ for better readability.

## Extended UTxO {#sec:eutxo}

The Hydra Head protocol is specified to work on the so-called Extended
UTxO (EUTxO) ledgers like Cardano.

The basis for EUTxO is Bitcoin's UTxO ledger
model [@formal-model-of-bitcoin-transactions; @Zahnentferner18-UTxO].
Intuitively, it arranges transactions in a directed acyclic graph, such
as the one in Figure [1](#fig:utxo-graph){reference-type="ref"
reference="fig:utxo-graph"}, where boxes represent transactions with
(red) inputs to the left and (black) outputs to the right. A dangling
(unconnected) output is an *unspent transaction output (UTxO)* --- there
are two UTxOs in the figure.

![Example of a plain UTxO graph](figures/utxo-graph.pdf){#fig:utxo-graph
width="50%"}

The following paragraphs will give definitions of the UTxO model and
it's extension to support scripting (EUTxO) suitable for this Hydra Head
protocol specification. For a more detailed introduction to the EUTxO
ledger model, see [@eutxo], [@eutxo-2] and [@utxo-ma].

### Values

<!-- FIXME: definitions do not work -->

::: definition
Values are sets that keep track of how many units of which tokens of
which currency are available. Given a finitely supported function
$\mapsto$, that maps keys to monoids, a value is the set of such
mappings over currencies (minting policy identifiers), over a mapping of
token names $t$ to quantities $q$:
$$\val \in \tyValue = (c : \tyBytes \mapsto (t : \tyBytes \mapsto q : \tyInteger))$$
where addition of values is defined as $+$ and $\varnothing$ is the
empty value.
:::

For example, the value
$\{c_{1} \mapsto \{t_1 \mapsto 1, t_2 \mapsto 1\}\}$ contains tokens
$t_1$ and $t_2$ of currency $c_{1}$ and addition merges currencies and
token names naturally:

$$
\begin{aligned}
         & \{c_{1} \mapsto \{t_1 \mapsto 1, t_2 \mapsto 1\}\}                                                        \\
    + \  & \{c_{1} \mapsto \{t_{2} \mapsto 1, t_3 \mapsto 1\}, c_{2} \mapsto \{ t_{1} \mapsto 2\}\}                  \\
    = \  & \{c_{1} \mapsto \{t_1 \mapsto 1, t_2 \mapsto 2, t_3 \mapsto 1\}, c_{2} \mapsto \{ t_{1} \mapsto 2\}\} \ .
\end{aligned}
$$

While the above definition should be sufficient for the purpose of this
specification, a full definition for finitely supported functions and
values as used here can be found in [@utxo-ma]. To further improve
readability, we define the following shorthands:

-   $\{t_1, \ldots, t_n\} :: c$ for a set positive single quantity
    assets $\{c \mapsto \{t_1 \mapsto 1, \ldots, t_n \mapsto 1\}\}$,

-   ${\{t_1, \ldots, t_n\}}^{-1} :: c$ for a set of negative single
    quantity assets
    $\{c \mapsto \{t_1 \mapsto -1, \ldots, t_n \mapsto -1\}\}$,

-   $\{c \mapsto t \mapsto q\}$ for the value entry
    $\{c \mapsto \{t \mapsto q\}\}$,

-   $\{c \mapsto \cdot \mapsto q \}$ for any asset with currency $c$ and
    quantity $q$ irrespective of token name.

### Scripts

Validator scripts are called *phase-2* scripts in the Cardano Ledger
specification (see [@ledger-alonzo-spec] for a formal treatment of
these). Scripts are used for multiple purposes, but most often (and
sufficient for this specification) as a *spending* or *minting* policy
script.

::: definition
A script $\mu \in \mathcal{M}$ governing whether a value can be minted
(or burned), is a pure function with type
$$\mu \in \mathcal{M} = (\rho : \tyData) \to (\txContext : \tyContext) \to\tyBool,$$
where $\rho \in \tyData$ is the redeemer provided as part of the
transaction being validated and $\txContext \in \tyContext$ is the
validation context.
:::

::: definition
A validator script $\nu \in \mathcal{V}$ governing whether an output can
be spent, is a pure function with type
$$\nu \in \mathcal{V} = (\delta : \tyData) \to (\rho : \tyData) \to (\txContext : \tyContext) \to\tyBool,$$
where $\delta \in \tyData$ is the datum available at the output to be
spent, $\rho \in \tyData$ is the redeemer data provided as part of the
transaction being validated, and $\txContext \in \tyContext$ is the
validation context.
:::

### Transactions

We define EUTxO inputs, outputs and transactions as they are available
to scripts and just enough to specify the behavior of the Hydra
validator scripts. For example outputs addresses and datums are much
more complicated in the full ledger
model [@eutxo-2; @ledger-shelley-spec].

::: definition
An output $o \in \txOutputs$ stores some value $\val \in \tyValue$ at
some address, defined by the hash of a validator script
$\nu^{\#} \in \tyBytes = \hash(\nu \in \mathcal{V})$, and may store
(reference) some data $\delta \in \tyData$:
$$o \in \txOutputs = (\val : \tyValue \times \nu^{\#} : \tyBytes \times \delta : \tyData)$$
:::

::: definition
An output reference $\txOutRef \in \tyOutRef$ points to an output of a
transaction, using a transaction id (that is, a hash of the transaction
body) and the output index within that transaction.
$$\txOutRef \in \tyOutRef = (\tyBytes \times \mathbb{N})$$
:::

::: definition
A transaction input $i \in \txInputs$ is an output reference
$\txOutRef \in \tyOutRef$ with a corresponding redeemer
$\rho \in \tyData$:
$$i \in \txInputs = (\txOutRef : \tyOutRef \times \rho : \tyData)$$
:::

::: definition
A validation context $\txContext \in \tyContext$ is a view on the
transaction to be validated:
$$\txContext \in \tyContext = (\tyInputs \times \tyOutputs \times \tyValue \times \mathcal{S}^{\leftrightarrow} \times \mathcal{K})$$
where $\txInputs \in \tyInputs$ is a **set** of inputs,
$\txOutputs \in \tyOutputs$ is a **list** of outputs,
$\txMint \in \tyValue$ is the minted (or burned) value,
$(\txValidityMin, \txValidityMax) \in \tyValidity$ are the lower and
upper validity bounds where $\txValidityMin <= \txValidityMax$, and
$\txKeys \in \mathcal{K}$ is the set of verification keys which signed
the transaction.
:::

Informally, scripts are evaluated by the ledger when it applies a
transaction to its current state to yield a new ledger state (besides
checking the transaction integrity, signatures and ledger rules). Each
validator script referenced by an output is passed its arguments drawn
from the output it locks and the transaction context it is executed in.
The transaction is valid if and only if all scripts validate, i.e.
$\mu(\rho, \txContext) = \true$ and
$\nu(\delta, \rho, \txContext) = \true$.

### State machines and graphical notation

State machines in the EUTxO ledger model are commonly described using
the *constraint emitting machine (CEM)* formalism [@eutxo], e.g. the
original paper describes the Hydra Head protocol using this
notation [@hydrahead20]. Although inspired by CEMs, this specification
uses a more direct representation of individual transactions to simplify
description of non-state-machine transactions and help translation to
concrete implementations on Cardano. The structure of the state machine
is enforced on-chain through *scripts* which run as part of the ledger's
validation of a transaction (see
Section [1.3](#sec:eutxo){reference-type="ref" reference="sec:eutxo"}).
For each protocol transaction, the specification defines the structure
of the transaction and enumerates the transaction constraints enforced
by the scripts ($\tx^\equiv$ in the CEM formalism).
