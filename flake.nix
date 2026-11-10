{
  description = "nmm-ocaml";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";

  outputs =
    { self, ... }@inputs:
    let
      inherit (inputs.nixpkgs) lib;

      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-freebsd"
      ];

      forEachSupportedSystem =
        f:
        lib.genAttrs supportedSystems (
          system:
          f {
            inherit system;
            pkgs = import inputs.nixpkgs {
              inherit system;
              config.allowUnfree = true;
            };
          }
        );
    in
    {

      devShells = forEachSupportedSystem (
        { pkgs, system }:
        {
          default = pkgs.callPackage ./shell.nix { };
        }
      );
 
      packages = forEachSupportedSystem (
        { pkgs, system }:
        {
          default = pkgs.callPackage ./default.nix { };
        }
      );

    };
}
