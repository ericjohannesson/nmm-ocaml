{
  description = "nmm-ocaml";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = inputs: {
    packages = builtins.mapAttrs (system: pkgs: {
      nmm-ocaml = pkgs.callPackage ./default.nix { };
      default = inputs.self.packages.${system}.nmm-ocaml;
    }) inputs.nixpkgs.legacyPackages;
  };
}
