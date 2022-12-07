# Build stages

FROM nixos/nix:2.12.0 as build

RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf && \
    echo "accept-flake-config = true" >> /etc/nix/nix.conf

WORKDIR /build
COPY . .

# hydra-node

FROM build as build-hydra-node
RUN nix build .#hydra-node-static -o hydra-node-result

FROM alpine as hydra-node
COPY --from=build-hydra-node /build/hydra-node-result/bin/hydra-node /bin/
STOPSIGNAL SIGINT
ENTRYPOINT ["hydra-node"]

# hydra-tools

FROM build as build-hydra-tools
RUN nix build .#hydra-tools-static -o hydra-tools-result

FROM alpine as hydra-tools
COPY --from=build-hydra-tools /build/hydra-tools-result/bin/hydra-tools /bin/
STOPSIGNAL SIGINT
ENTRYPOINT ["hydra-tools"]

# hydra-tui

FROM build as build-hydra-tui
RUN nix build .#hydra-tui-static -o hydra-tui-result

FROM alpine as hydra-tui
RUN apk update && apk add ncurses-terminfo
COPY --from=build-hydra-tui /build/hydra-tui-result/bin/hydra-tui /bin/
STOPSIGNAL SIGINT
ENTRYPOINT ["sh", "-c", "TERMINFO=/usr/share/terminfo hydra-tui $@", "--"]

# hydraw

FROM build as build-hydraw
RUN nix build .#hydraw-static -o hydraw-result

FROM alpine as hydraw
COPY --from=build-hydraw /build/hydraw-result/bin/hydraw /bin/
COPY hydraw/index.html index.html
COPY hydraw/bundle.js bundle.js
COPY hydraw/style.css style.css
COPY hydraw/logo.png logo.png
STOPSIGNAL SIGINT
ENTRYPOINT ["hydraw"]
