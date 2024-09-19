{
  description = "Description for the project";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        # To import a flake module
        # 1. Add foo to inputs
        # 2. Add foo as a parameter to the outputs function
        # 3. Add here: foo.flakeModule

      ];
      systems = [ "x86_64-linux" "aarch64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        # Per-system attributes can be defined here. The self' and inputs'
        # module parameters provide easy access to attributes of the same
        # system.

        devShells.default = pkgs.mkShell {
          name = "apollopp";
          nativeBuildInputs = with pkgs; [
            # For helper scripts
            python3
            # For translating Processing projects
            processing
            # For running the extractors
            maven
            rascal
            # For running the graphmatcher
            dotnet-sdk_8
          ];

          # Environment variables
          # The Processing core library is included in the package
          PROCESSING_CORELIB = "${pkgs.processing}/share/processing/core/library/core.jar";
          # Additionally installed libraries are in this path by default on Linux
          PROCESSING_LIBRARIES = "~/sketchbook/libraries";
        };
      };
      flake = {
        # The usual flake attributes can be defined here, including system-
        # agnostic ones like nixosModule and system-enumerating ones, although
        # those are more easily expressed in perSystem.

        # TODO: Override rascal version
        # overlays.default = final: prev: {

        # };
      };
    };
}
