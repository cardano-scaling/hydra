# This image is really a _starting point_. Some paths for improvements:
#
# - Produce a static linux binary using musl
# - Find a way to split the build in multiple steps which can be cached (by Docker) better. Possibly not using Nix.

# ------------------------------------------------------------------------ BUILD

FROM nixos/nix:2.3.11 as build

WORKDIR /build

RUN echo "substituters = https://cache.nixos.org https://iohk.cachix.org https://hydra.iohk.io" >> /etc/nix/nix.conf &&\
  echo "trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" >> /etc/nix/nix.conf

COPY . .

RUN nix-build -A hydra-node -o hydra-node-result release.nix > hydra-node.drv
RUN nix-build -A hydra-tui -o hydra-tui-result release.nix > hydra-tui.drv
RUN nix-build -A hydraw -o hydraw-result release.nix > hydraw.drv

RUN nix-store --export $(nix-store -qR $(cat hydra-node.drv)) > hydra-node.closure
RUN nix-store --export $(nix-store -qR $(cat hydra-tui.drv)) > hydra-tui.closure
RUN nix-store --export $(nix-store -qR $(cat hydraw.drv)) > hydraw.closure

# ------------------------------------------------------------------- HYDRA-NODE

FROM nixos/nix:2.3.11 as hydra-node

COPY --from=build /build/hydra-node.drv hydra-node.drv
COPY --from=build /build/hydra-node.closure hydra-node.closure

RUN nix-store --import < hydra-node.closure && nix-env -i $(cat hydra-node.drv)

STOPSIGNAL SIGINT
ENTRYPOINT ["hydra-node"]

# ------------------------------------------------------------------- HYDRA-TUI

FROM nixos/nix:2.3.11 as hydra-tui

COPY --from=build /build/hydra-tui.drv hydra-tui.drv
COPY --from=build /build/hydra-tui.closure hydra-tui.closure

RUN nix-store --import < hydra-tui.closure && nix-env -i $(cat hydra-tui.drv)


STOPSIGNAL SIGINT
ENTRYPOINT ["hydra-tui"]

# ------------------------------------------------------------------- HYDRAW

FROM nixos/nix:2.3.11 as hydraw

COPY --from=build /build/hydraw.drv hydraw.drv
COPY --from=build /build/hydraw.closure hydraw.closure
COPY --from=build /build/hydraw/index.html index.html
COPY --from=build /build/hydraw/bundle.js bundle.js
COPY --from=build /build/hydraw/style.css style.css
COPY --from=build /build/hydraw/logo.png logo.png

RUN nix-store --import < hydraw.closure && nix-env -i $(cat hydraw.drv)

STOPSIGNAL SIGINT
ENTRYPOINT ["hydraw"]
