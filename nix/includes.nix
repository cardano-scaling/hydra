{ inputs, ... }: {

  imports = [
    inputs.hydra-coding-standards.flakeModule
    inputs.process-compose-flake.flakeModule
  ];

}
