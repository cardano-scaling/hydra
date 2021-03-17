Some attempt at modelling Hydra head protocol using π-calculus like notation.

## Notation

I have used "traditional" π-calculus notation with a lot of hand-waving and some abuse for iteration over `n` instances:

- `ν x.P` creates new channel and binds it to variable `x` then proceeds as P
- `p (x).P` receives a message and binds (or pattern-match) it to `x` then proceeds as `P`
- `p <x>.P` sends message `x` over `p` then proceeds as `P`
- `P || Q` is parallel composition of 2 processes
- `0` stops a process
- `!P` spawns infinitely many instances of process `P`.

## Transaction Emitter

`TxEmitter` is a process that takes as parameters a `client` channel and `n` channels to communicate with other nodes:

```
TxEmitter (client, p_1 ... p_n) =
```

then starts infinitely many transaction (`tx`) handlers. Input construct is a binder so here `tx` is a fresh variable whose content will be the received transaction payload:

```
  !( client(tx).
```

broadcast reqTx message to all other parties. If message sending is really asynchronous then
there should not be a continuation after sending but let's keep things simple

```
     (p_i < (req, tx) > { i = 1 .. n }).
```

concurrently wait for all `ackTx` messages. Here, `ack` and `tx` _should not_ be fresh variables
but actually patterns the message is matched on, eg. we are only interested in the `ackTx` message for the
current `tx` of interest.

```
    ((|| p_i ((ack,tx))) { i = 1 .. n }).
```

then broadcast confirmation and stop:

```
   (p_i < (conf, tx) > { i = 1 .. n }).0
  )
```

## Transaction Observer

`TxObserver` is a a process that takes as parameters `n` channels to communicat with other nodes:

```
TxObserver (p_1 .. p_n) =
```

Then it starts infinitely many transaction observation handlers on each of the `p_i` channels

```
  !( Handler(p_1) || ... || Handler(p_n))
  where
```

The `Handler` process then handles confirmation of a single transaction and stops:

```
   Handler(p) =
       p ((req,tx)) .
       p < (ack, tx) > .
       p ((conf, tx)).0
```

### MUX

The role of the `Mux` process is then to handle proper dispatching of messages through underlying transport.
We assume the transport messages carry the identity (channel) of interest when receiving it but still needs
`n` channels as parameters for egress messages:

```
Mux(transport, p_1, ..., p_n) =
```

We both need to retrieve ingress messages and dispatch them to the right channel, with infinitely many replicas:

```
  !(transport((msg, p)) . p < msg >.0)
  ||
```

composed in parallel with egress messages dispatching over the `n` channels:

```
  !( || p_i (msg) . transport < (msg, i) >.0 {i = 1 .. n} )
```

### Node

A node then results from the composition of those processes following creation of proper channels

```
ν transport.
  ν client .
    ν p_i { i = 1 .. n } .
     (TxEmitter(client, p_1 .. p_n) || TxObserver(p_1 .. p_n) || Mux(transport, p_1 .. p_n)).0
```
