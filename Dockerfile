# Build stages

FROM nixos/nix:2.3.11 as build

RUN echo "substituters = https://cache.nixos.org https://cache.iog.io" >> /etc/nix/nix.conf &&\
  echo "trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" >> /etc/nix/nix.conf

WORKDIR /build
COPY . .

# hydra-node

FROM build as build-hydra-node
RUN nix-build -A hydra-node-static -o hydra-node-result release.nix

FROM alpine as hydra-node
COPY --from=build-hydra-node /build/hydra-node-result/bin/hydra-node /bin/
STOPSIGNAL SIGINT
ENTRYPOINT ["hydra-node"]

# hydra-tools

FROM build as build-hydra-tools
RUN nix-build -A hydra-tools-static -o hydra-tools-result release.nix

FROM alpine as hydra-tools
COPY --from=build-hydra-tools /build/hydra-tools-result/bin/hydra-tools /bin/
STOPSIGNAL SIGINT
ENTRYPOINT ["hydra-tools"]

# hydra-tui

FROM build as build-hydra-tui
RUN nix-build -A hydra-tui-static -o hydra-tui-result release.nix

FROM alpine as hydra-tui
COPY --from=build-hydra-tui /build/hydra-tui-result/bin/hydra-tui /bin/
STOPSIGNAL SIGINT
ENTRYPOINT ["hydra-tui"]

# hydraw

FROM build as build-hydraw
RUN nix-build -A hydraw-static -o hydraw-result release.nix

FROM alpine as hydraw
COPY --from=build-hydraw /build/hydraw-result/bin/hydraw /bin/
COPY hydraw/index.html index.html
COPY hydraw/bundle.js bundle.js
COPY hydraw/style.css style.css
COPY hydraw/logo.png logo.png
STOPSIGNAL SIGINT
ENTRYPOINT ["hydraw"]
