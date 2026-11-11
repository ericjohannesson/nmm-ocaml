{
  description = "An implementation of no-markup-markup in OCaml";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
  };

  outputs =
    { nixpkgs, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
    {

      devShells = nixpkgs.lib.genAttrs systems (system:
        {
          default = nixpkgs.legacyPackages.${system}.callPackage ./shell.nix { };
        }
      );
 
      packages = nixpkgs.lib.genAttrs systems (system:
        {
          default = nixpkgs.legacyPackages.${system}.callPackage ./default.nix { };
        }
      );

    };
}
