{
  description = "nmm-ocaml";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
  };

  outputs =
    i:
    let
      lib = i.nixpkgs.lib;

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      syspkgs = i.nixpkgs.legacyPackages;

    in
    {

      devShells = lib.genAttrs systems (system:
        {
          default = syspkgs.${system}.callPackage ./shell.nix { };
        }
      );
 
      packages = lib.genAttrs systems (system:
        {
          default = syspkgs.${system}.callPackage ./default.nix { };
        }
      );

    };
}
