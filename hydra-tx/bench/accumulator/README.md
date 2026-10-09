# Accumulator Benchmark Suite

This benchmark suite measures the performance characteristics of the BLS accumulator implementation for the Hydra protocol, specifically for snapshot signing.

## Running the Benchmarks

### Basic Run

```bash
cabal bench accumulator-bench
```

### With Memory Profiling

```bash
cabal bench accumulator-bench --benchmark-options '+RTS -T'
```

This will show peak memory allocation in the output.

### Generate JSON Report

```bash
cabal bench accumulator-bench --benchmark-options '--json results.json'
```

Then analyze with Python (results.json is a single-line array):
```bash
python3 << 'EOF'
import json
with open('results.json') as f:
    data = json.load(f)
    for item in data[2]:  # results are in data[2]
        if isinstance(item, dict) and 'Build Accumulator' in item.get('reportName', ''):
            print(f"{item['reportName']}: {item['reportAnalysis']['anMean']['estPoint']*1000:.2f} ms")
EOF
```


### Detailed Output

```bash
cabal bench accumulator-bench --benchmark-options '-v'
```

## Benchmark Groups

### 1. Build Accumulator from UTxO
Measures the time to build an accumulator from different UTxO sizes (10 to 10,000).

**Key Metric**: Should be < 100ms for 1000 UTxOs to be viable for snapshot signing.

### Delta update (one output replaced)

Measures `applyUTxODelta` with one output removed and one added under a fresh
reference. The other outputs remain unchanged, so this includes the cost of
checking their contents. The UTxO sets and previous accumulator map are evaluated
before timing; computing the new G1 commitment is excluded.

```bash
BENCH_MAX_UTXO=4000 cabal bench hydra-tx:bench:accumulator-bench \
  --benchmark-options='--time-limit 0.2 --resamples 100 --match prefix "Delta update" +RTS -N1 -RTS'
```

### 2. UTxO to Elements Conversion
Measures the overhead of extracting and serializing TxOuts for accumulator elements.

### 3. Create Membership Proofs
Measures the time to create cryptographic proofs that a subset of UTxOs exists in the full set.

**Key Metric**: Critical for partial fanout performance.

### 4. Create Membership Proofs (Low-level)
Direct measurement of proof generation from pre-serialized elements.

### 5. Accumulator Hashing
Measures the time to compute the hash of an accumulator for snapshot signing.

### 6. Accumulator Serialization
Measures serialization overhead and size.

### 7. CRS Generation
Measures the one-time cost of generating the Common Reference String.

**Note**: This is typically done once at startup, not in the critical path.

### 8. End-to-End Snapshot Simulation
Simulates complete workflows:
- Full snapshot cycle (build + hash + serialize)
- Partial fanout cycle (build + prove)

## Advanced Usage

### Profile Specific Sizes

Focus on realistic production sizes:
```bash
cabal bench accumulator-bench --benchmark-options '-m pattern "1000"'
```

### Generate Plots

Using criterion's built-in plotting (requires gnuplot):
```bash
cabal bench accumulator-bench --benchmark-options '--output benchmark.html'
```
