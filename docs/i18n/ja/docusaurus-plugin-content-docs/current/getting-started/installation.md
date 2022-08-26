---
sidebar_position: 2
---

# インストール

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

> ハイドラの入手先は？

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

## ソースからの構築

docker を使用することが Hydra を _use_ するための推奨される方法ですが、ソースから `hydra-node` をビルドすることもできます。 ただし、これには [nix](https://nixos.org/download.html) を使用することをお勧めします。手順については、[寄稿ガイドライン](https://github.com/input-output-hk/hydra-poc/blob/master/CONTRIBUTING.md) を参照してください。
