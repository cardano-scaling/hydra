# Hydra Specification

The Hydra Head protocol is specified in the single document built from this
directory.

We do use [pandoc][pandoc] to generate a PDF from the markdown sources via
XeLaTeX and also prepare a markdown suitable to be embedded into our
[website](../docs) using docusaurus.

The big benefit here is that we can use [latex macros][latex-macros] and the
powerful [diagram][diagram] extensions to use [TikZ][tikz] and
[mermaid][mermaid] diagrams for a unified experience.

Any changes to the specification should follow the common pull request rules as
set out in our [contribution guidelines](../CONTRIBUTING.md).

## Building

You can use `nix` to build the specification using

``` shell
nix build .#spec
```

which will create the PDF specifcation in `result/hydra-spec.pdf` and the fully
composed markdown version in `result/hydra-spec.md`.

Alternatively, you would need to have a [pandoc][pandoc], a LaTeX distribution
(e.g. `texlive`), as well as tools to generate diagrams (`graphviz`, `mmdc`)
installed to produce the same output with

``` shell
./build.sh result
```

[pandoc](https://pandoc.org)
[latex-macros](https://pandoc.org/MANUAL.html#latex-macros)
[diagram](https://github.com/pandoc-ext/diagram)
[tikz](https://en.wikibooks.org/wiki/LaTeX/PGF/TikZ)
[mermaid](https://mermaid.js.org)
