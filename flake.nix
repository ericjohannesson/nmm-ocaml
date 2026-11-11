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

      pkgs_of_system =
        sys:
        import i.nixpkgs {
          system = sys;
        };
    in
    {

      devShells = lib.genAttrs systems (sys:
        {
          default = (pkgs_of_system sys).callPackage ./shell.nix { };
        }
      );
 
      packages = lib.genAttrs systems (sys:
        {
          default = (pkgs_of_system sys).callPackage ./default.nix { };
        }
      );

    };
}
