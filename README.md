# <p align="center">:construction: Hydra Proof of Concept (POC) 🚧</p>

<div align="center">
  <p>A home to our colorful experiments and prototypes.</p>
  <a href='https://github.com/input-output-hk/hydra-poc/actions'><img src="https://img.shields.io/github/workflow/status/input-output-hk/hydra-poc/CI?label=Tests&style=for-the-badge" /></a>
</div>

## :warning: Disclaimer

This is prototypical and exploratory work shared here for your interest.
Although we might create a prototype for a `hydra-node` in here, some
experiments have not a clear goal or usage in mind. We do not provide
documentation or support for the artifacts created here by purpose as we
also intend to throw away / discontinue use when we start building the
final product. Please note the code quality seen around here is not
representative of our best practices and you will find many code smells
and dirtyhacks. Thanks for visiting and enjoy!

## 👷‍♂️ Development

We provide a `shell.nix` to set up a development environment. So a simple call
to `nix-shell` should put everything in place for building, testing and
general development.

Make sure the following caches are listed in your `nix.conf` for a speedy setup:

```
substituters = https://cache.nixos.org https://iohk.cachix.org https://hydra.iohk.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

Also, some of us use [direnv](https://direnv.net/) and
[nix-direnv](https://github.com/nix-community/nix-direnv) to automatically
import the nix-shell environment into our favorite shell or editor.

### Building & Testing

`cabal build` and `cabal test` should work as expected

## Documentation

Documentation is published online at https://input-output-hk.github.io/hydra-poc from the [docs](docs/README.md) directory.

[Haddock](https://www.haskell.org/haddock/) documentation is also published for each package:
* [hydra-node](https://input-output-hk.github.io/hydra-poc/haddock/hydra-node/index.html)
* [hydra-plutus](https://input-output-hk.github.io/hydra-poc/haddock/hydra-plutus/index.html)
* [local-cluster](https://input-output-hk.github.io/hydra-poc/haddock/local-cluster/index.html)
