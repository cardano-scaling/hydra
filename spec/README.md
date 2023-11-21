---
title: "Hydra HeadV1 Specification: Coordinated Head protocol"
subtitle: DRAFT
classoption:
  - 11pt
documentclass: article
author: "Sebastian Nagel sebastian.nagel@iohk.io"
geometry: "left=4cm, right=3cm, top=2.5cm, bottom=2.5cm"
numbersections: true
boxlinks: true

bibliography: short.bib
reference-section-title: References
link-bibliography: true
link-citations: true
csl: elsevier-with-titles.csl
---

# Hydra Specification

The Hydra Head protocol is specified in the single document built from this
directory.

We use LaTeX to write the specification and used to compose this on
[overleaf](www.overleaf.com). Going forward, any changes to the specification
should follow the common pull request rules as set out in our [contribution
guidelines](../CONTRIBUTING.md).

The [Overleaf documentation](https://www.overleaf.com/learn) does provide a good
introduction to LaTeX syntax and functions to get started.

## Building

You can use `nix` to build a PDF from the LaTeX files using

``` shell
nix build .#spec
```

which will write the PDF into `result/hydra-spec.pdf`.

Alternatively, you would need to have a LaTeX distribution installed (e.g.
`texlive`) and produce a PDF output using `pdflatex` etc. The [LaTeX
wikibook](https://en.wikibooks.org/wiki/LaTeX/Basics#Building_a_document) might
be helpful to set things up.
