---
sidebar_position: 10
---

# Deposit with the JavaScript SDK

Use the SDK to sign a deposit drafted by an open Hydra head. The node adds its
fuel-wallet witness; the client adds the spending witness and submits the
complete transaction to Cardano. The API endpoint remains `POST /commit`.

## Set up

Use Node.js 22 or later, a preprod Hydra node with an open head and fuel, a
funded preprod wallet, and a Blockfrost preprod project. This example pins the
SDK version so its signing behavior can be reproduced.

```shell
mkdir deposit-demo
cd deposit-demo
npm init -y
npm install --save-exact @hydra-sdk/core@1.1.5 dotenv@17.2.3 ws@8.18.3
npm install --save-dev --save-exact tsx@4.21.0 typescript@5.9.3 @types/ws@8.18.1 @types/node@22
```

Keep the generated lockfile. Put these values in `.env`, using a wallet
created only for test funds. Exclude `.env` from version control.

```text
BLOCKFROST_API_KEY=your_preprod_project_id
WALLET_MNEMONIC=your test wallet mnemonic
HYDRA_API=http://127.0.0.1:4001
HYDRA_WS=ws://127.0.0.1:4001
```

Save `config.ts`:

```typescript
import 'dotenv/config'
import { AppWallet, NETWORK_ID, ProviderUtils } from '@hydra-sdk/core'

function required(name: string): string {
  const value = process.env[name]
  if (!value) throw new Error(`Set ${name} in .env`)
  return value
}

const provider = new ProviderUtils.BlockfrostProvider({
  apiKey: required('BLOCKFROST_API_KEY'),
  network: 'preprod',
})

export const wallet = new AppWallet({
  key: { type: 'mnemonic', words: required('WALLET_MNEMONIC').trim().split(/\s+/) },
  networkId: NETWORK_ID.PREPROD,
  fetcher: provider.fetcher,
  submitter: provider.submitter,
})

export async function post(path: string, body: string) {
  const response = await fetch(`${process.env.HYDRA_API ?? 'http://127.0.0.1:4001'}${path}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body,
  })
  const text = await response.text()
  if (!response.ok) throw new Error(`${response.status}: ${text}`)
  return text ? JSON.parse(text) : null
}
```

## Observe settlement

Save `monitor.ts` and run it in a separate terminal **before** submitting a
deposit. It does not initialize or use the wallet.

```typescript
import 'dotenv/config'
import WebSocket from 'ws'

const url = new URL(process.env.HYDRA_WS ?? 'ws://127.0.0.1:4001')
url.searchParams.set('history', 'no')
const socket = new WebSocket(url)
const depositEvents = new Set([
  'CommitRecorded', 'DepositActivated', 'CommitApproved',
  'CommitFinalized', 'DepositExpired', 'CommitRecovered',
])

socket.on('open', () => console.log('Listening for deposit events'))
socket.on('error', console.error)
socket.on('message', data => {
  const event = JSON.parse(data.toString())
  if (depositEvents.has(event.tag)) console.log(event)
})
```

```shell
npx tsx monitor.ts
```

`CommitRecorded` means the deposit was observed on L1. `DepositActivated`
means it is eligible for inclusion; `CommitApproved` means all parties signed
the snapshot. **Wait for `CommitFinalized` for your deposit transaction ID**
before spending its outputs. Default activation takes roughly an hour plus
the upper-validity offset. See [deposit timing](../configuration.md#deposit-activation).

## Sign and submit

Save `deposit.ts`. With no argument it deposits one ADA-only UTxO from the
wallet. With a filename it sends that deposit request unchanged, preserving
exact integer values in its JSON.

```typescript
import { readFile } from 'node:fs/promises'
import { Converter } from '@hydra-sdk/core'
import { post, wallet } from './config'

async function main() {
  let request: string
  if (process.argv[2]) {
    request = await readFile(process.argv[2], 'utf8')
  } else {
    const address = wallet.getAccount().baseAddressBech32
    const utxos = await wallet.queryUTxOs(address)
    const selected = utxos.find(utxo => {
      const assets = utxo.output.amount
      return assets.length === 1 && assets[0].unit === 'lovelace'
        && BigInt(assets[0].quantity) >= 5_000_000n
        && BigInt(assets[0].quantity) <= BigInt(Number.MAX_SAFE_INTEGER)
    })
    if (!selected) throw new Error('Fund an ADA-only UTxO of at least 5 ADA')
    request = JSON.stringify(Converter.convertUTxOToUTxOObject([selected]))
  }

  const draft = await post('/commit', request)
  // Partial signing adds a witness and preserves the node's existing witness.
  const signed = await wallet.signTx(draft.cborHex, true)
  const result = await post('/cardano-transaction', JSON.stringify({
    type: 'Tx ConwayEra',
    description: 'Signed deposit',
    cborHex: signed,
  }))
  console.log('Submitted to L1:', result)
  console.log('Wait for CommitFinalized before spending on L2')
}

main().catch(error => {
  console.error(error)
  process.exitCode = 1
})
```

```shell
npx tsx deposit.ts
```

In this SDK version, `signTx(cbor, true)` returns a complete transaction with
an added witness, not a standalone witness set. Do not change the transaction
body or output order after the node drafts it.

## Blueprint and script deposits

The same signing code accepts a request produced by the
[partial-deposit tutorial](./commit-blueprint.md) or
[script-UTxO tutorial](./commit-script-utxo.md):

```shell
npx tsx deposit.ts commit-request.json
```

Build the blueprint using the actual inputs owned by this wallet, or arrange
all other required witnesses before submission. Include the UTxO context for
its spending and reference inputs. Use explicit outputs when including
reference-input context: a blueprint without outputs deposits the entire
supplied UTxO map. Explicit blueprint outputs become L2
outputs; `changeAddress` returns the remaining input value on L1. Do not add
an L1 change output to the blueprint itself unless you intend to deposit it.

For a script deposit, preserve its script address and datum if it must remain
script-controlled on L2. A blueprint that pays a key address changes that
ownership. Follow [dApp validator guidance](./best-practise-dapp.md) for
application-specific checks.

## Recovery and errors

Inspect HTTP errors before retrying. `DepositTooLow` means the recorded
outputs need more ADA to cover the deposit output's inline datum;
`DepositTooLarge` means the deposit cannot be claimed within L1 limits. A
submitted deposit that expires may be recovered after its deadline, subject
to the [rollback restrictions](./incremental-commit.md#recover-a-deposit).
