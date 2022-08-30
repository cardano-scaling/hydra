# Interpreting Benchmark Runs

When running a benchmark using `cabal bench hydra-cluster` it creates log files in its `output-directory` beside a `results.csv`.
Those log files can be used to harvest some more data about the run and produce more graphs. The following notes require `jq` and a few standard tools from GNU [coreutils](https://github.com/coreutils/coreutils) like `cat`, `sed`, `awk` and friends.

**NOTE**: The following scripts all work over the _filtered logs_ which are produced by the `log-filter` program.

#### Number of tx/snapshot

Computes the number of transactions / snapshot processed:

```
cat log.1 | grep ReqSn | grep NetworkEvent | grep EndEvent | jq -r '[.timestamp, (.message.event.message.transactions| length)] | @csv' | tr -d \" > snapshot-length.csv
```

### ReqSn processing time

Computes the processing time of each `ReqSn` event:

First produce a CSV file containing the start/stop time for each snapshot:

```
$ cat log.1 | grep ReqSn | grep NetworkEvent | grep 'BeginEvent' | jq -r '[.message.event.message.snapshotNumber, .timestamp] | @csv' | tr -d \" > snapshot-processing.csv
$ cat log.1 | grep ReqSn | grep NetworkEvent | grep 'EndEvent' | jq -r '[.message.event.message.snapshotNumber, .timestamp] | @csv' | tr -d \" > snapshot-processed.csv
$ join -t ',' snapshot-processing.csv snapshot-processed.csv > snapshot-time.csv
```

Then we can produce a graph plotting the `ReqSn` execution time over time:

```
set title 'Evolution of ReqSn Execution time'
set xdata time
set timefmt "%Y-%m-%dT%H:%M:%SZ"
set datafile separator ","
set term pngcairo size 2048,1532
set output 'snapshot-time.png'
plot 'snapshot-time.csv' u (timecolumn(2)):((timecolumn(3) - timecolumn(2))) t Execution time (s)'
```

It is straightforward to apply the same technique for all other types of events and effect, changing the filters.

### Snapshot confirmation time

An interesting piece of information one can extract from the logs is how long does it take for a snapshot to be signed by all nodes. This is more involved as we need to:
* Extract the start and end time of all `AckSn` received per snapshot
* Compute the difference between the minimum of start time of the first and the maximum of end time.

The following script extracts the `processing-` and `processed-` times for each snapshot number, one file per _party_ (assuming there were 6 nodes hence 6 log files produced):
```
$ for i in {1..6}; do cat log.1 | grep AckSn | grep NetworkEvent | grep BeginEvent | jq -r "select (.message.event.message.party == $i) | [.message.event.message.snapshotNumber, .timestamp] | @csv" | tr -d \" > processing-ack-$i.csv ; done
$ for i in {1..6}; do cat log.1 | grep AckSn | grep NetworkEvent | grep EndEvent | jq -r "select (.message.event.message.party == $i) | [.message.event.message.snapshotNumber, .timestamp] | @csv" | tr -d \" > processed-ack-$i.csv ; done
join -t ',' processing-ack-1.csv processing-ack-2.csv | join -t ',' - processing-ack-3.csv
```

We then Join all `AckSn` timings into a single file `ack-all.csv`:

```
join -t ',' processed-ack-1.csv processed-ack-2.csv | join -t ',' - processed-ack-3.csv | join -t ',' - processed-ack-4.csv | join -t ',' - processed-ack-5.csv| join -t ',' - processed-ack-6.csv > processed-ack-all.csv
join -t ',' processing-ack-1.csv processing-ack-2.csv | join -t ',' - processing-ack-3.csv | join -t ',' - processing-ack-4.csv | join -t ',' - processing-ack-5.csv| join -t ',' - processing-ack-6.csv > processing-ack-all.csv
join -t ',' processing-ack-all.csv processed-ack-all.csv > ack-all.csv
```

I manually transformed this file back to JSON in Emacs before processing its content using nodejs:

```.js
const fs = require('fs');
const ack = fs.readFileSync('ack-sn.json');
const d = JSON.parse(ack);
const ts = d.map(arr => [arr[0]].concat(arr.slice(1).map(d => new Date(d).getTime())));
const minmax = ts.map(arr => [arr[0],Math.min(...arr.slice(1,6)), Math.max(...arr.slice(7))]);
const sntime = minmax.map(arr => [new Date(arr[2]),arr[2] - arr[1]]);
fs.writeFileSync('ack-sn.json',JSON.stringify(sntime));
```

Then producing a CSV for plotting with gnuplot amounts to:

```
cat ack-sn.json | jq -rc '.[] | @csv' | tr -d \" > ack-sn.csv
```

gnuplot is a bit quirky to work with if the data is not in the right format, doing computation and transformations on data is awkward, in our case computing the [moving average](http://skuld.bmsc.washington.edu/~merritt/gnuplot/canvas_demos/running_avg.html) for receiving all `AckSn`. Here is a gnuplot script doing the trick for 10 points:

```
set xdata time
set format x "%H:%M:%S"
set xtics out rotate
set title 'Snapshot acknowledgement time (ms) - 1389 snapshots'
samples(x) = $0 > 9 ? 10 : ($0+1)
avg10(x) = (shift10(x), (back1+back2+back3+back4+back5+back6+back7+back8+back9+back10)/samples($0))
shift10(x) = (back10 = back9, back9 = back8, back8 = back7, back7 = back6, back6 = back5, back5 = back4, back4 = back3, back3 = back2, back2 = back1, back1 = x)
init(x) = (back1 = back2 = back3 = back4 = back5 = back6 = back7 = back8 = back9 = back10 = sum = 0)
plot sum =init(0),\
 'ack-sn.csv' u (timecolumn(1,"%Y-%m-%dT%H:%M:%SZ")):($2)  w l t 'AckSn Processing time (ms)', \
 'ack-sn.csv' u (timecolumn(1,"%Y-%m-%dT%H:%M:%SZ")):(avg10($2)) w l t 'Moving average (10 points)', \
 'ack-sn.csv' u (timecolumn(1,"%Y-%m-%dT%H:%M:%SZ")):(sum = sum + $2, sum/($0+1)) w l t 'Cumulative mean'
```

### Growth of UTXO size

This one a is a bit involved as it requires both extracting information from the logs and reconstructing state from the transactions themselves.

* Extract timestamp for each transction in the `confirmedTransactions` field of a snapshot confirmed message but turns out it's pretty simple:
  ```
  $ cat log.1 | grep EndEffect | grep SnapshotConfirmed | jq -cr '{ts:.timestamp, txs: .message.effect.serverOutput.snapshot.confirmedTransactions[]}' | jq -cr '[.txs, .ts]' > tx-confirmed-time.json
  ```
* Generate a UTXO size growth per transaction id (see [this haskell script](./utxo-size.hs))
  ```
  cd hydra-cluster
  cabal run runghc utxo-size.hs -- tx-confirmed-time.json test/dataset.json
  ```
* Convert the 2 JSON files to CSV (exercise left to the reader):
  ```
  cat utxo-size.json | jq -cr '.[] | @csv' > utxo-size.csv
  cat confirmed-txs.json | jq -cr '.[] | @csv' > confirmed-txs.csv
  ```
* Combine the two pieces into a single data file:
  ```
  $ join -t ',' utxo-size.csv confirmed-txs.csv  | cut -d ',' -f 2- | tr -d \" | awk -F ',' '{ print $2, $1 }' | sort > utxo-time.csv
  ```
* Plot the data using this gnuplot script:
  ```
  set title "UTXO Size over Time"
  set term pngcairo size 2048,1532
  set output "utxo-size.png"
  set xlabel "Time"
  set xtics out rotate
  set xdata time
  set format x "%H:%M:%S"
  set ylabel "# UTXO"
  plot 'utxo-time'.csv u  (timecolumn(1,"%Y-%m-%dT%H:%M:%SZ")):2 w l t 'UTXO Set'
  ```
