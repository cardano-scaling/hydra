---
sidebar_label: "ベンチマーク"
sidebar_position: 1
---

# ベンチマークと制限事項

この項目では、Hydra Head オンチェーンプロトコルの既知の制限に関する最新データを提供します。Cardano のトランザクション（およびブロック）には、トランザクションサイズ、実行コスト、Input と Output の数に制限があり、これらはネットワークパラメータに依存し Head プロトコルの機能に影響を及ぼします。Head に参加できるパーティーの数、Head にコミットできる UTxO の数、ファンアウトできる数はオンチェーンスクリプトとトランザクションが成熟し、最適化され、基礎となるカルダノチェーンが、より多くのパラメータとより効率的なスクリプトの実行によって進化すると、これらの制限は変更されることになります。

これらのページで提供されるデータは、Hydra の [統合プロセス](https://github.com/input-output-hk/hydra/actions/workflows/ci-nix.yaml)によって生成されるため、コードの現在の状態を反映することが保証されています。

```mdx-code-block
import DocCardList from '@theme/DocCardList';
import {useDocsSidebar} from '@docusaurus/theme-common/internal';

<DocCardList items={useDocsSidebar().items.filter(({ docId }) => docId != "index")}/>
```
