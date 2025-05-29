{ inputs, ... }: {

  perSystem = { system, ... }: {
    packages.spec = inputs.hydra-spec.packages.${system}.default;
  };

}
