use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{VerificationKey}

type Datum {
}

type Redeemer {
}

validator {
  fn commit(datum: Datum, redeemer: Redeemer, context: ScriptContext) -> Bool {
    True
  }
}
