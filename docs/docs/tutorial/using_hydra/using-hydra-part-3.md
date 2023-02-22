---
sidebar_position: 6
---

# Running

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

## Opening a head

Now that the nodes are running, we can use them. Each node opened a Web Socket API, for Alice via port `4001` and for Bob via port `4002`. Using postman, a client for api communication, we can connect to each. To achieve this, in Postman, go to new -> WebSocketRequest. Enter as the server URL `localhost:4001` for Alice her node, then press connect. When connected, you will see messages appear. Also make a separate connection for Bob via `localhost:4002`.

![postman-setup-view.png](../images/postman-setup-view.png)

Now that we are connected, we can use this API to communicate with each Hydra head. For a detailed API reference, see the [documentation](https://hydra.family/head-protocol/api-reference).

To open a head, one of the two parties first has to initialize the state channel. This can be done via the following API call

```json
{
	"tag": "Init",
	"contestationPeriod": 60
}
```
Here the `contestationPeriod` is the time the other parties have when a checkpoint is posted on the mainchain and the head is closing. Practically, this means that any party has 60 seconds after that event to contest and post their latest snapshot to the mainchain. The hydra node will contest automatically in case of an outdated snapshot in the closing stage.

We post this JSON message as Alice in the "new message" field in postman. When sent, we very via Bob that he received the message and saw the initialize transaction onchain,
```json
{
   "parties": [
       {
           "vkey": "7037e4d3612be471027bb5ce7a18b47d975f13e86abd9fc3c242454b6b44b761"
       },
       {
           "vkey": "8d9db0de7e06b2e4205b49640a0ab7f487d06dc1def2a25e472dbcca5b831d8a"
       }
   ],
   "tag": "ReadyToCommit"
}
```
These `"vkey"` fields are the Hydra keys of each party. Now Alice and Bob can commit their funds to the head, this can be done via the API call
```json
{
	"tag": "Commit",
	"utxo": {
   		"fe199aeb379ae6cdb5933d5abd690a3cf420246d21b4631e93238a9541406773#0": {
   			"address": "addr_test1vq4rdnygedqe76nkaj2kpdajhl43sys448lhjpyru6xtyzc4p7zj5",
   			"value": {
      			"lovelace": 100000000
   			}
   		}
	}
}
```
Here, we replace the field of the tag `"utxo"` with the utxo that each party wants to add. Using the commands
```
cardano-cli query utxo --address $(cat ./test-head/Alice/AliceCardano.addr) --testnet-magic 2
cat test-head/Alice/AliceCardano.addr 
cardano-cli query utxo --address $(cat ./test-head/Bob/BobCardano.addr) --testnet-magic 2
cat test-head/Bob/BobCardano.addr
```
we extract the information needed to fill in the `"utxo"` fields for both parties. Do not commit the UTxO that is marked as fuel, it has a `TxOutDatumHash ScriptDataInBabbageEra` at its output. Currently, it also not possible to commit a UTxO partially, currently this action has to be done before initializing the wallets.

Note that we can also commit no UTxO at all by leaving this field blank, in this case the party will still perform the protocol and sign checkpoints but without the initial commitment of any funds. 
	
So, for both Alice and Bob, we send a `"commit"` API call that adds funds to the head. It is necessary that both do this to open a head! Without all parties having done this action, the protocol cannot process. In such case, any party can abort, and retrieve their committed funds, via the abort API call
```json
{
	"tag": "Abort"
}
```
After each party has committed something, the other party will be informed via a message given by its node. Like this, for example
```json
{
   	"party": {
   	    "vkey": "ff18415d4d2b49718e20ca7d24e893dd87c943eb5cd17e1ee36b9c714ff183b2"
   	},
   	"tag": "Committed",
   	"utxo": {
   	    "0f5d9bc80894a3938d67d78336ecfa437d7272de52bb3303556cefd282fe1e20#0": {
   	        "address": "addr_test1vrwnl84mn56q6ffx06qu58kvxpk399fal627h37lfjwy40cxykgkv",
   	        "datum": null,
   	        "datumhash": null,
   	        "inlineDatum": null,
   	        "referenceScript": null,
   	        "value": {
   	            "lovelace": 4899832651
   	        }
   	    }
   	}
}
```
After each party has made the commit action, the `hydra-node` will automatically send the transaction to the mainchain that will collect all these funds and will open the head. This is logged by the Web Socket as
```json
{
   	"tag": "HeadIsOpen",
   	"utxo": {
       	"0f5d9bc80894a3938d67d78336ecfa437d7272de52bb3303556cefd282fe1e20#0": {
           	"address": "addr_test1vrwnl84mn56q6ffx06qu58kvxpk399fal627h37lfjwy40cxykgkv",
           	"datum": null,
           	"datumhash": null,
           	"inlineDatum": null,
           	"referenceScript": null,
           	"value": {
               	"lovelace": 4899832651
           	}
       	},
       	"4c91b2c9c77fa84c20be57c535cb2f9f8945f296f52b777ac4b42e99e985fec4#0": {
           	"address": "addr_test1vqneq3v0dqh3x3muv6ee3lt8e5729xymnxuavx6tndcjc2cv24ef9",
           	"datum": null,
           	"datumhash": null,
           	"inlineDatum": null,
           	"referenceScript": null,
           	"value": {
               	"lovelace": 4899832651
           	}
       	}
   	}
}
```
This also indicated the balance as reported by the first checkpoint. Notice that the UTxOs of the initial snapshot correspond to the initial committed UTxO's (even the Tx hash and Tx id are the same). The head is now open and ready to be used!

## Utilizing a head

Now that the hydra head is open, we want to make a basic transaction between Alice and Bob. Since hydra is an isomorphic state channel, most things that work on the mainchain also work in the head. This means that constructing transactions is no different from the mainchain. This is great since it allows the usages of already written smart contracts and the use of already existing tools! In this simple example head, we will send 1000 ada from Bob to Alice.

To start we query the API for the current state of the UTxO set, we do this via
```json
{
	"tag": "GetUTxO"
}
```
From it, we extract the UTxO that has the address of Bob in its `"address"` field. Then, just like on the mainchain, we construct a transaction via the `cardano-cli` that spends this UTxO. We send it to the address of Alice via
```bash
cardano-cli transaction build-raw \
	--tx-in 0f5d9bc80894a3938d67d78336ecfa437d7272de52bb3303556cefd282fe1e20#0 \
	--tx-out addr_test1vqneq3v0dqh3x3muv6ee3lt8e5729xymnxuavx6tndcjc2cv24ef9+1000000000 \
	--tx-out addr_test1vrwnl84mn56q6ffx06qu58kvxpk399fal627h37lfjwy40cxykgkv+3899832651 \
	--fee 0 \
	--out-file tx.raw
```
Notice that we need to use the `build-raw` version, since the client cannot index this UTxO in the mainchain (it will give an error that is does not exist). Also, when we set up the protocol parameters for the head we choose them to have zero fee's, that is why we can use the `--fee 0` flag. Then we sign this transaction with Bobs Cardano verification key
```bash
cardano-cli transaction sign --tx-body-file tx.raw --signing-key-file ./test-head/Bob/BobCardano.sk --out-file tx.signed
cat tx.signed
```
Here we did not specify in either command the network magic, since hydra is not a blockchain but a state channel. To send this transaction in the hydra head, we copy the `"cborHex"` field and add this to the `"transaction"` field of the following API call from either hydra nodes
```json
{
	"tag": "NewTx",
	"transaction": "84a300818258200f5d9bc80894a3938d67d78336ecfa437d7272de52bb3303556cefd282fe1e2000018282581d602790458f682f13477c66b398fd67cd3ca2989b99b9d61b4b9b712c2b1a3b9aca0082581d60dd3f9ebb9d340d25267e81ca1ecc306d12953dfe95ebc7df4c9c4abf1ae872b94b0200a100818258205ef70cf2ef40cec074a3835daa95c133a00faca8a70143a837e28585203db6815840b584465704a250f515c86efbcf3705c7deae82132de62103173f3ab7fe838a59e071025e2e6c57150f575bd838acbb76323cea197f23f2aab827c07507705905f5f6"
}
```
Once send, there will be a few responses from the hydra node. The first is the conformation from the node that just send the transaction, it assesses if it was a valid transaction (the other nodes do not see this log).
```json
{
   	"tag": "TxValid",
   	"transaction": {
       	"body": {
           	"fees": 0,
           	"inputs": [
               	"0f5d9bc80894a3938d67d78336ecfa437d7272de52bb3303556cefd282fe1e20#0"
           	],
           	"outputs": [
               	{
                   	"address": "addr_test1vqneq3v0dqh3x3muv6ee3lt8e5729xymnxuavx6tndcjc2cv24ef9",
                   	"datum": null,
                   	"datumhash": null,
                   	"inlineDatum": null,
                   	"referenceScript": null,
                   	"value": {
                       	"lovelace": 1000000000
                   	}
               	},
               	{
                   	"address": "addr_test1vrwnl84mn56q6ffx06qu58kvxpk399fal627h37lfjwy40cxykgkv",
                   	"datum": null,
                   	"datumhash": null,
                   	"inlineDatum": null,
                   	"referenceScript": null,
                   	"value": {
                       	"lovelace": 3899832651
                   	}
               	}
           	]
       	},
       	"id": "28deef61b098c4608bfc9913dbe1488072c9c289d4c0bdf165db58320439ebf9",
       	"isValid": true,
       	"witnesses": {
           	"keys": [
               	"82008258205ef70cf2ef40cec074a3835daa95c133a00faca8a70143a837e28585203db6815840b584465704a250f515c86efbcf3705c7deae82132de62103173f3ab7fe838a59e071025e2e6c57150f575bd838acbb76323cea197f23f2aab827c07507705905"
           	]
       	}
   	}
}
```
Then every other node (even the node that send the transaction) logs that is sees a new transaction as
```json
{
    "tag": "TxSeen",
    "transaction": {
        "body": {
            "fees": 0,
            "inputs": [
                "0f5d9bc80894a3938d67d78336ecfa437d7272de52bb3303556cefd282fe1e20#0"
            ],
            "outputs": [
                {
                    "address": "addr_test1vqneq3v0dqh3x3muv6ee3lt8e5729xymnxuavx6tndcjc2cv24ef9",
                    "datum": null,
                    "datumhash": null,
                    "inlineDatum": null,
                    "referenceScript": null,
                    "value": {
                        "lovelace": 1000000000
                    }
                },
                {
                    "address": "addr_test1vrwnl84mn56q6ffx06qu58kvxpk399fal627h37lfjwy40cxykgkv",
                    "datum": null,
                    "datumhash": null,
                    "inlineDatum": null,
                    "referenceScript": null,
                    "value": {
                        "lovelace": 3899832651
                    }
                }
            ]
        },
        "id": "28deef61b098c4608bfc9913dbe1488072c9c289d4c0bdf165db58320439ebf9",
        "isValid": true,
        "witnesses": {
            "keys": [
                "82008258205ef70cf2ef40cec074a3835daa95c133a00faca8a70143a837e28585203db6815840b584465704a250f515c86efbcf3705c7deae82132de62103173f3ab7fe838a59e071025e2e6c57150f575bd838acbb76323cea197f23f2aab827c07507705905"
            ]
        }
	   }
}
```
Followed by a message that there is a new snapshot made. Here, each party automatically sign and processes any valid update of the state in the head. After all, the new transaction was a valid transaction on the previous UTxO set.

A good thing to remember is that these transactions and snapshots are not bound by block production, as on the mainchain. This means that the speed at which transactions can be processed is completely limited by the internet connection of the peers in the head and the number of peers that need to sign for each checkpoint.

## Closing a head

When a party is finished with the head and wishes to close it, they can do so at any time. This means that they can do this even if others are not done with the head yet. To close a head, you can use the API call

```json
{
	"tag": "Close"
}
```
After this call, a transaction on the mainchain will be made with the latest know snapshot of the party that closed the head. The other nodes will notice this onchain transaction and log
```json
{
   	"contestationDeadline": "2022-11-07T13:08:22Z",
    "snapshotNumber": 1,
    "tag": "HeadIsClosed"
}
```
If the snapshot number is smaller than the latest know snapshot of the other nodes, they have time to contest until `"2022-11-07T13:08:22Z"`. A hydra node will automatically contest in the case of an incorrect snapshot in the close transaction. Once this deadline has passed, the node will log
```json
{
   "tag": "ReadyToFanout"
}
```
which means that everybody can now grab their funds on the mainchain. They do this by calling the API with
```json
{
  "tag": "Fanout"
}
```

Now, if the final state of the latest snapshot is sufficient large, the fanout transaction might go over the maximum transaction size of the mainchain. This is why this fanout transaction might consist of multiple transactions. In our simple example, the state is not that large and with one transaction, we can pay out the UTxO's to both parties. The hydra node now also logs that the head is finalized
```json
{
    "tag": "HeadIsFinalized",
    "utxo": {
        "28deef61b098c4608bfc9913dbe1488072c9c289d4c0bdf165db58320439ebf9#0": {
            "address": "addr_test1vqneq3v0dqh3x3muv6ee3lt8e5729xymnxuavx6tndcjc2cv24ef9",
            "datum": null,
            "datumhash": null,
            "inlineDatum": null,
            "referenceScript": null,
            "value": {
                "lovelace": 1000000000
            }
        },
        "28deef61b098c4608bfc9913dbe1488072c9c289d4c0bdf165db58320439ebf9#1": {
            "address": "addr_test1vrwnl84mn56q6ffx06qu58kvxpk399fal627h37lfjwy40cxykgkv",
            "datum": null,
            "datumhash": null,
            "inlineDatum": null,
            "referenceScript": null,
            "value": {
                "lovelace": 3899832651
            }
        },
        "4c91b2c9c77fa84c20be57c535cb2f9f8945f296f52b777ac4b42e99e985fec4#0": {
            "address": "addr_test1vqneq3v0dqh3x3muv6ee3lt8e5729xymnxuavx6tndcjc2cv24ef9",
            "datum": null,
            "datumhash": null,
            "inlineDatum": null,
            "referenceScript": null,
            "value": {
                "lovelace": 4899832651
            }
        }
    }
}
```
The protocol is now over. We can check onchain for the final balances of Alice and Bob to see that they indeed received the correct funds from the head.
```bash
cardano-cli query utxo --address $(cat ./test-head/Alice/AliceCardano.addr) --testnet-magic 2
cardano-cli query utxo --address $(cat ./test-head/Bob/BobCardano.addr) --testnet-magic 2
```
