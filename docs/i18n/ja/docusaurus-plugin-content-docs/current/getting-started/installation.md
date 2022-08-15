---
sidebar_position: 2
---

# インストール手順

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
import DocumentMetadata from '@site/src/components/DocumentMetadata';
```

```mdx-code-block
<DocumentMetadata />
```

> Hydraノードのインストール手順です。クイックスタートとしてDockerを使用し、コンテナを扱うことをお勧めします。

## Dockerの使用
hydra-nodeを動かす一番迅速な方法は、専用のDockerイメージを使うことです。

````mdx-code-block
<TerminalWindow>

```
docker pull ghcr.io/input-output-hk/hydra-node:latest
docker run --rm ghcr.io/input-output-hk/hydra-node --help
```

</TerminalWindow>
````

:::tip
通常はこれで十分です。 ソースコードからすべてをインストールする場合を除いて、次の項目をスキップして、[クイックスタート](/docs/getting-started/quickstart)に移動してください
:::

## Nixの使用

開発環境を構築するために、`shell.nix`を提供しています。そのため、nix-shellを呼び出すだけで、ビルド、テスト、および一般的な開発に必要なすべての環境が整います。
迅速にセットアップするために、`nix.conf`に以下のキャッシュがリストされていることを確認してください。

```nix title="nix.conf"
substituters = https://cache.nixos.org https://iohk.cachix.org https://hydra.iohk.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

また、`direnv` や `nix-direnv` を使って、nix-shell 環境をお気に入りのシェルやエディタに自動的にインポートしたりキャッシュしたりすることも可能です。
nix-shell内で、`cabal build` と `cabal test` は意図した通りに動作します。

また、`nix-build` を使ってプロジェクトとすべての実行ファイルをビルドすることができます。ビルド後は、`result/bin/` にあります。

## Cabalの使用

1. ghcup などを使って、基本的な Haskell 開発環境をインストールします。Hydra は GHC 8.10.7 と最近の cabal (> 3.0) を必要とします。

1. 依存環境プログラムをインストールします（Debian系）。

    ````mdx-code-block
    <TerminalWindow>

    ```
    sudo apt install -y  build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
    sudo apt install -y  libz-dev liblzma-dev libzmq3-dev pkg-config libtool
    ```

    </TerminalWindow>
    ````

    `lzma` と `liblzma-dev` を混同しないでください、これらは個別の既存パッケージです。

1. フォークされたlibsodiumをインストールします。

    ````mdx-code-block
    <TerminalWindow>

    ```
    git clone https://github.com/input-output-hk/libsodium
    cd libsodium/
    git checkout 66f017f16633f2060db25e17c170c2afa0f2a8a1
    ./autogen.sh
    ./configure
    make && sudo make install
    ```

    </TerminalWindow>
    ````

1. 統合テストやベンチマークを実行するには、最新の `cardano-node` をインストールする必要があります。[公式ドキュメント](https://developers.cardano.org/docs/get-started/installing-cardano-node)を参照してください。

1. 全てを構築してテストします。

    ```mdx-code-block
    <TerminalWindow>
    cabal build all && cabal test all
    </TerminalWindow>
    ```
