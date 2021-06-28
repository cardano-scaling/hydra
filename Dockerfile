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
RUN nix-build -A mock-chain -o mock-chain-result release.nix > mock-chain.drv
RUN nix-build -A hydra-pab  -o hydra-pab-result  release.nix > hydra-pab.drv

RUN nix-store --export $(nix-store -qR $(cat hydra-node.drv)) > hydra-node.closure
RUN nix-store --export $(nix-store -qR $(cat mock-chain.drv)) > mock-chain.closure
RUN nix-store --export $(nix-store -qR $(cat hydra-pab.drv))  > hydra-pab.closure

# ------------------------------------------------------------------- HYDRA-NODE

FROM nixos/nix:2.3.11 as hydra-node

COPY --from=build /build/hydra-node.drv hydra-node.drv
COPY --from=build /build/hydra-node.closure hydra-node.closure

RUN nix-store --import < hydra-node.closure && nix-env -i $(cat hydra-node.drv)

ENTRYPOINT ["hydra-node"]

# ------------------------------------------------------------------- MOCK-CHAIN

FROM nixos/nix:2.3.11 as mock-chain

COPY --from=build /build/mock-chain.drv mock-chain.drv
COPY --from=build /build/mock-chain.closure mock-chain.closure

RUN nix-store --import < mock-chain.closure && nix-env -i $(cat mock-chain.drv)

ENTRYPOINT ["mock-chain"]

# -------------------------------------------------------------------- HYDRA-PAB

FROM nixos/nix:2.3.11 as hydra-pab

COPY --from=build /build/hydra-pab.drv hydra-pab.drv
COPY --from=build /build/hydra-pab.closure hydra-pab.closure

RUN nix-store --import < hydra-pab.closure && nix-env -i $(cat hydra-pab.drv)

ENTRYPOINT ["hydra-pab"]
