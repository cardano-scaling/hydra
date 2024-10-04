# Hydra plutus scripts

Most of our scripts are built using `plutus-tx` which are automatically built
when compiling the whole package. However, some validators are implemented using `aiken` and embedded into the Haskell library from the plutus blueprint in `plutus.json`.

## Build

It's important that we keep the traces of the aiken validators with `-t`:

```sh
aiken build -t compact
cabal build
```

The resulting `plutus.json` is deliberately checked in and doubles as a golden
file for our tests.

## Test

```sh
cabal test
```
