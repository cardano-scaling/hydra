# hydra-formal-specification

Agda specification for the hydra-protocol.

## Building

To produce the specification PDF in `result/`:

```
nix build
```

## Developing / writing

In a nix shell (`nix develop` or using `nix-direnv`) you can type check:
```sh
agda src/Hydra/Protocol/Main.lagda
```

or build the PDF iteratively:
``` sh
shake
```

with the specification PDF available in `_build/hydra-spec.pdf`.
