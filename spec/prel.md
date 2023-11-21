# Preliminaries {#sec:prel}

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
application of a function $f$ to an argument $x$ is written as $f(x)$.\
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

TODO
