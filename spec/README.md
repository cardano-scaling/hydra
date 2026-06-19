# hydra-formal-specification

Literate-Agda + Typst specification for the hydra-protocol.

The prose, math and figures are written in [Typst](https://typst.app); the source
files are literate Typst (`.lagda.typ`) so that Agda code blocks are type-checked.
Code fences render as follows (see `src/template.typ`):

- ` ```agda ` blocks are type-checked by Agda **and** shown in the PDF.
- bare ` ``` ` blocks (module declarations, imports) are type-checked but hidden.

## Building

To produce the specification PDF (via `nix build .#spec`):

```
nix build .#spec
```

The PDF is written to `result/hydra-spec.pdf`.

## Developing / writing

In a nix shell (`nix develop` or using `nix-direnv`) you can type-check the Agda:

```sh
agda src/Hydra/Protocol/Main.lagda.typ
```

or build the PDF iteratively with the build script (Agda typecheck + Typst render):

```sh
./build.sh
```

with the specification PDF available in `_build/hydra-spec.pdf`.
