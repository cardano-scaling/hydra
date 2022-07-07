---
sidebar_position: 99
---

# Haskellパッケージ

Hydraプロジェクトは、プロトコルのさまざまな部分を満たす複数のHaskellパッケージに分かれています。一部のパッケージは内部的でHydraプロジェクトに限定されたものですが、非常に汎用的で、同様の問題に直面している他のプロジェクトにも有用かもしれません。 いずれにしても [Haddock](https://www.haskell.org/haddock/) ドキュメントに全てを公開します。 

## 公開パッケージ

| パッケージ                                                                                        | 概要                                                                         |
| ---                                                                                            | ---                                                                                 |
| [plutus-merkle-tree](https://hydra.family/head-protocol/haddock/plutus-merkle-tree/index.html) | オンチェーンPlutusバリデータと互換性があるMerkle Treesの実装         |
| [plutus-cbor](https://hydra.family/head-protocol/haddock/plutus-cbor/index.html)               | オンチェーンPlutusバリデータと互換性のあるCBORエンコーダの実装       |
| [hydra-prelude](https://hydra.family/head-protocol/haddock/hydra-prelude/index.html)           | 他のHydraパッケージで使用されるカスタムHydra導入部                           |
| [hydra-cardano-api](https://hydra.family/head-protocol/haddock/hydra-cardano-api/index.html)   | 期間特化型と追加ユーティリティを持つ `cardano-api` ラッパー|

## 内部パッケージ

| パッケージ                                                                                   | 概要                                                            |
| ---                                                                                        | ---                                                                     |
| [hydra-node](https://hydra.family/head-protocol/haddock/hydra-node/index.html)             | Hydraノード.                                                         |
| [hydra-node tests](https://hydra.family/head-protocol/haddock/hydra-node/tests/index.html) | Hydraテストノード.                                               |
| [hydra-tui](https://hydra.family/head-protocol/haddock/hydra-tui/index.html)               | Hydraノードを管理するためのターミナルユーザーインターフェイス（TUI）                 |
| [hydra-plutus](https://hydra.family/head-protocol/haddock/hydra-plutus/index.html)         | Hydra Plutusコントラクト                                                 |
| [hydra-cluster](https://hydra.family/head-protocol/haddock/hydra-cluster/index.html)       | Cardanoとhydraノードのローカルクラスタを用いた統合テストパッケージ |
