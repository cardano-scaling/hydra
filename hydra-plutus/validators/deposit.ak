use cardano/assets.{PolicyId}
use aiken/interval.{Finite, Interval, IntervalBound}
use cardano/transaction.{OutputReference, ValidityRange, Transaction, Output}

pub type Datum {
  Datum {currencySymbol: ByteArray, deadline: Int, commits: List<OutputReference>}
}

pub type Redeemer {
  Claim (PolicyId)
  Recover(Int)
}

type DepositError {
    DepositDeadlineSurpassed
    DepositNoUpperBoundDefined
    DepositNoLowerBoundDefined
    DepositDeadlineNotReached
    IncorrectDepositHash
    WrongHeadIdInDepositDatum
}

fn toErrorCode(err: DepositError) -> String {
  when err is {
    DepositDeadlineSurpassed  -> @"D01"
    DepositNoUpperBoundDefined -> @"D02"
    DepositNoLowerBoundDefined -> @"D03"
    DepositDeadlineNotReached -> @"D04"
    IncorrectDepositHash -> @"D05"
    WrongHeadIdInDepositDatum -> @"D06"
  }
}

validator deposit {
  spend(datum: Option<Datum>, redeemer: Redeemer, _utxo: OutputReference, self:Transaction ) -> Bool {
    expect Some(datum) = datum
    when redeemer is {
      Claim(currencySymbol) ->
        traceIfFalse(
          toErrorCode(WrongHeadIdInDepositDatum),
          check_head_id(datum.currencySymbol, currencySymbol)
        ) &&
        traceIfFalse(
          toErrorCode(DepositDeadlineSurpassed),
          before_deadline(self.validity_range, datum.deadline)
        )
      Recover(n) -> {
        traceIfFalse(
          toErrorCode(DepositDeadlineNotReached),
          after_deadline(self.validity_range, datum.deadline)
        ) && recover_outputs(n, datum.commits, self.outputs )
      }
    }
  }
}

// Helpers

fn check_head_id(datumCS, redeemerCS){
  datumCS == redeemerCS
}

fn before_deadline(range: ValidityRange, dl){
  when range.upper_bound.bound_type is {
    Finite(tx_upper_validity) -> tx_upper_validity <= dl
    _ ->
        traceIfFalse(
          toErrorCode(DepositNoUpperBoundDefined),
          False,
        )
  }
}

fn after_deadline(range: ValidityRange, dl){
  when range.lower_bound.bound_type is {
    Finite(tx_lower_validity) -> tx_lower_validity > dl
    _ ->
        traceIfFalse(
          toErrorCode(DepositNoLowerBoundDefined),
          False,
        )
  }
}

fn recover_outputs(_n: Int, _commits: List<OutputReference>, _outputs: List<Output>){
  True
}

fn traceIfFalse(traceLog: String, predicate: Bool) -> Bool {
  if predicate {
    True
  } else {
    trace traceLog
    False
  }
}
