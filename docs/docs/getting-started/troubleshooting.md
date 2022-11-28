# Troubleshooting

## Known issues

- QueryException "AcquireFailurePointNotOnChain"

    + Description: It ocurs when you attempt to start a head using a point in the past too old (exceeding **k** limit).

    + Reference: [439](https://github.com/input-output-hk/hydra/issues/439) AcquireFailurePointTooOld when `--start-chain-from` point is past **k**

    + Workaround: Restart your node fresh, without state. For that you need to remove your persistance dir and restart the hydra-node.

- Hydra node crashes after a fork

    + Description: It ocurs during a fork and expects the operator to restart its hydra-node.

    + Reference: [560](https://github.com/input-output-hk/hydra/issues/560) Hydra node crashed after a fork
    
    + Workaround: Restarting your node should be enough to come back to live. Beware, if you wait to long to restart it then you may fall under `QueryAcquireException AcquireFailurePointTooOld` and will require you to restart without state.
