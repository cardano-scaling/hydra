Edit the API endpoint in the `bundle.js` to your API host+port, e.g. for `13.38.35.167`

``` javascript
const client = new WebSocket("ws://13.38.35.167:4001");
```

Launching the UI
``` sh
simple-http-server -oi
```

Launching the pixel-painting bridge

``` sh
cabal run pixel-painting
```
