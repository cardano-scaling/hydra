# This image is really a _starting point_. Some paths for improvements:
#
# - Use multi-stage docker to get a smaller "runtime" image.
# - Produce a static linux binary using musl
# - Find a way to split the build in multiple steps which can be cached (by Docker) better. Possibly not using Nix.

FROM nixos/nix:2.3.11

WORKDIR /build

RUN echo "substituters = https://cache.nixos.org https://iohk.cachix.org https://hydra.iohk.io" >> /etc/nix/nix.conf &&\
  echo "trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" >> /etc/nix/nix.conf

COPY . .

RUN nix-build -A hydra-node.components.exes.hydra-node && nix-collect-garbage

ENTRYPOINT ["result/bin/hydra-node"]
