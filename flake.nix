{
  description = "nmm-ocaml";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = inputs: {
    packages = builtins.mapAttrs (system: pkgs: {
      default = pkgs.callPackage ./. { };
    }) inputs.nixpkgs.legacyPackages;
  };
}
