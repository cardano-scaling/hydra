# Hydra Specification

The Hydra Head protocol is specified in the single document built from this
directory.

We do use [pandoc][pandoc] to generate a PDF from the markdown sources via
XeLaTeX, as well as embed it into our [website](../docs) using docusaurus.

Any changes to the specification should follow the common pull request rules as
set out in our [contribution guidelines](../CONTRIBUTING.md).

## Building

You can use `nix` to build the specification using

``` shell
nix build .#spec
```

which will create the PDF specifcation in `result/hydra-spec.pdf` and the fully
composed markdown version in `result/hydra-spec.md`.

Alternatively, you would need to have a [pandoc][pandoc] and a LaTeX
distribution installed (e.g. `texlive`) to produce the same output with

``` shell
./build.sh result
```

[pandoc][https://pandoc.org]
