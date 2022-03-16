All command from within `pixel-painting/` and assuming your Hydra API host+port is `hydra.example.io:4001` and your cardano signing key is `cardano.sk`:

Edit the API endpoint in the `bundle.js` to your API host+port, e.g.

``` javascript
const HYDRA_API_HOST = "hydra.example.io:4001";
```

Launching the pixel-painting bridge (assumes current directory is the `pixel-painting` directory):

``` sh
cabal build
HYDRA_API_HOST=hydra.example.io:4001 HYDRA_SIGNING_KEY=cardano.sk cabal exec pixel-painting
```
