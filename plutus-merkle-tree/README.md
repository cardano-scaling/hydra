# Plutus Merkle Tree [![](https://img.shields.io/badge/haddock-1.0.0-blue?style=for-the-badge&logo=haskell)](https://input-output-hk.github.io/hydra-poc/haddock/plutus-merkle-tree/Plutus-MerkleTree.html)

A Plutus-compatible implementation of Merkle tree data-structure for compact membership checking. 

## Getting Started

### Off-Chain

```hs
import Plutus.MerkleTree (MerkleTree)
import qualified Plutus.MerkleTree as MT

foo :: MerkleTree 
foo = MT.fromList (serialize <$> [1,2,3,4,5])

proof :: MT.Proof 
proof = fromJust $ MT.mkProof (serialize 1) foo
```

### On-chain

```hs
import qualified Plutus.MerkleTree as MT

data MerkleTreeValidator

instance ValidatorTypes MerkleTreeValidator where
  type DatumType MerkleTreeValidator = ()
  type RedeemerType MerkleTreeValidator = (BuiltinByteString, MT.Hash, MT.Proof)

merkleTreeValidator :: TypedValidator MerkleTreeValidator
merkleTreeValidator =
  mkTypedValidator @MerkleTreeValidator
    $$( Plutus.compile
          [||
          \() (e, root, proof) _ctx ->
            member e root proof
          ||]
      )
    $$(Plutus.compile [|| wrap ||])
 where
  wrap = wrapValidator @(DatumType MerkleTreeValidator) @(RedeemerType MerkleTreeValidator)
```

## Performance

Costs are given relatively to maximum execution budgets, as per mainnet's parameters in December 2021.

### `member`

| Size | Relative Memory Cost (%) | Relative CPU Cost (%) |
| ---  | ---                      | ---                   |
| 1    | 3.79926                  | 1.66382747            |
| 5    | 4.29852                  | 1.94672544            |
| 10   | 4.50449                  | 2.06363924            |
| 20   | 4.71046                  | 2.18055305            |
| 50   | 4.98234                  | 2.33487927            |
| 100  | 5.18831                  | 2.45179308            |
| 200  | 5.39428                  | 2.56870688            |
| 300  | 5.51269                  | 2.63589112            |
| 400  | 5.60025                  | 2.68562069            |
| 500  | 5.64942                  | 2.71389113            |

### `fromList`

| Size | Relative Memory Cost (%) | Relative CPU Cost (%) |
| ---  | ---                      | ---                   |
| 1    | 4.154640                 | 1.768791440           |
| 2    | 5.458540                 | 2.334479400           |
| 3    | 7.910040                 | 3.411735080           |
| 4    | 10.79170                 | 4.630768740           |
| 5    | 15.54140                 | 6.732053050           |
| 6    | 20.72126                 | 8.975115340           |
| 7    | 26.33128                 | 11.35995561           |
| 8    | 32.37146                 | 13.88657386           |
| 9    | 41.71756                 | 18.03591543           |
| 10   | 51.49382                 | 22.32703498           |
| 20   | 195.9213                 | 84.88358210           |
