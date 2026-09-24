// Build a Hydra deposit datum entirely client-side, with no hydra-node /commit.
//
// The deposit output carries an inline datum of Hydra's `DepositDatum`:
//   (CurrencySymbol headId, POSIXTime deadline, [Commit])
// where each Commit is (TxOutRef input, ByteString preSerializedOutput) and the
// preSerializedOutput is the CBOR of the deposited output as a Plutus V3 TxOut.
//
// The node round-trip-checks these bytes (it re-serialises with Haskell cborg),
// so the encoding must match exactly: constructors as tag 121+i over an
// indefinite array (empty ones as a definite empty array), lists as indefinite
// arrays, and maps as definite. @harmoniclabs/cbor gives that byte-level control;
// generic Plutus-Data encoders emit indefinite maps and fail the check.
//
// This builder handles the common case: depositing an ada-only UTxO owned by a
// key (enterprise address, no staking part) whole into the head.
import { writeFileSync } from 'node:fs'
import { Cbor, CborArray, CborMap, CborBytes, CborUInt, CborTag } from '@harmoniclabs/cbor'

const bytes = (h) => new CborBytes(h === '' ? new Uint8Array(0) : Uint8Array.from(Buffer.from(h, 'hex')))
const int = (n) => new CborUInt(BigInt(n))
const list = (xs) => new CborArray(xs, { indefinite: true })
const map = (pairs) => new CborMap(pairs.map(([k, v]) => ({ k, v })))
// Plutus constructor: tag 121+i; non-empty -> indefinite array, empty -> definite.
const constr = (i, fs) => new CborTag(BigInt(121 + i), new CborArray(fs, { indefinite: fs.length > 0 }))
// Cbor.encode returns a Uint8Array in some versions and a CborString in others.
const encode = (o) => { const c = Cbor.encode(o); return Buffer.from(typeof c?.toBuffer === 'function' ? c.toBuffer() : c) }

const arg = (name) => {
  const i = process.argv.indexOf('--' + name)
  if (i < 0 || i + 1 >= process.argv.length) throw new Error('missing --' + name)
  return process.argv[i + 1]
}

const headId = arg('head-id')
const deadlineMs = BigInt(arg('deadline-ms'))
const txid = arg('txid')
const ix = BigInt(arg('ix'))
const pkh = arg('pkh')
const amount = BigInt(arg('amount'))
const out = arg('out')

// The deposited output, as a Plutus V3 TxOut: Constr 0 [address, value, datum, refScript].
const address = constr(0, [constr(0, [bytes(pkh)]), constr(1, [])]) // pubkey cred, no staking
const value = map([[bytes(''), map([[bytes(''), int(amount)]])]]) // ada only
const txOut = constr(0, [address, value, constr(0, []), constr(1, [])]) // NoDatum, no ref script
const preSerializedOutput = encode(txOut).toString("hex")

const commit = constr(0, [constr(0, [bytes(txid), int(ix)]), bytes(preSerializedOutput)])
const datum = constr(0, [bytes(headId), int(deadlineMs), list([commit])])

const cbor = encode(datum)
writeFileSync(out, cbor)
process.stdout.write(cbor.toString('hex') + '\n')
