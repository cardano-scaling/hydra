---
title: "Hydra HeadV1 Specification: Coordinated Head protocol"
subtitle: DRAFT
classoption:
  - 11pt
documentclass: article
author: "Sebastian Nagel sebastian.nagel@iohk.io"
geometry: "left=4cm, right=3cm, top=2.5cm, bottom=2.5cm"
numbersections: true
---

---
abstract: |

    Your abstract here 

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

# Intro {#sec:intro}

To use this template, open a new folder and type: `nix flake init -t pandoc-xelatex`. Then type `nix build` to produce a PDF from `README.md` and edit the `flake.nix` to use other files. The template can already use citations [@nixosWebsite] and references to sections, e.g. to sec [@sec:intro], figures, etc ...

# References

---
references:
- id: nixosWebsite
  author: NixOS Community
  title: Main Website
  url: https://nixos.org
  type: online

---
