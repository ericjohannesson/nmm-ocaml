{ pkgs ? import (
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/26.05.tar.gz";
      sha256 = "sha256-md0zn0RnwNvPyASas1yG5YUuwQ4ALA6ucL50l0DvqCo=";
    }
  ) { }
}:
let
  build_pkgs = with pkgs; [
    ocaml
    ocamlPackages.findlib
    ocamlPackages.sedlex
    ocamlPackages.uuseg
    ocamlPackages.xml-light
  ];
  dev_pkgs = with pkgs; [
    ocamlPackages.utop
    ocamlformat
    gh
    gh-markdown-preview
  ];
in
pkgs.mkShell {
  packages = build_pkgs ++ dev_pkgs;
}
