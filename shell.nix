{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = with pkgs;
    [
      ocaml
      ocamlPackages.utop
      ocamlPackages.findlib
      ocamlPackages.sedlex
      ocamlPackages.uuseg
      ocamlPackages.xml-light
    ];
}
