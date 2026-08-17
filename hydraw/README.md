# Hydraw

![Painting a Bison with Hydra](./hydraw.gif)

_Hydraw_ is the result of a 3 days Hackathon that took place in Lyon March 2022. It's a simple web application that can connect to a Hydra node's Client API. It provides the following features:

* Participants in the Head can request coloring individual pixels using the provided user interface which results in a _transaction_ being posted into the Head,
* Metadata from _Confirmed_ transactions happening in the Head is interpreted as instructions to draw individual pixels with some RGB color.

```diff
- DISCLAIMER
! This code is the result of an intense 3 days long Hackathon and is not meant for long-term maintenance and
! evolution, hence it should not be taken as representative of the coding practices the Hydra team members
! endorse in the rest of the codebase.
```

# Build

To build the web server that connects serves frontend and connects to Hydra API:

```
cabal build
```

The frontend is written in vanilla HTML/CSS/JS and is dynamically served from the web server. The latter also provides a WebSocket "proxy" to the Hydra node's Client API.

# Run

All commands must be run from within `hydraw/` and assuming your Hydra API host+port is `hydra.example.io:4001` and your cardano signing key is `cardano.sk`:

You can choose the network by specifying `HYDRAW_NETWORK`.

This value contains either network magic number in case you want to run hydraw on one of the testnets or a string "mainnet".

In case HYDRAW_NETWORK is not set we default to `Testnet (NetworkMagic 42)`.

In case the magic number parsing fails we assume you want to run on Mainnet!

To launch the `hydraw` bridge:

``` sh
HYDRA_API_HOST=hydra.example.io:4001 HYDRAW_CARDANO_SIGNING_KEY=cardano.sk HYDRAW_NETWORK=2 cabal exec hydraw
```
