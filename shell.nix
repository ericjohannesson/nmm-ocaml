{ pkgs ? import <nixpkgs> { } }:
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
  shellHook = ''
    echo "Welcome to the developer shell for nmm-ocaml!"
    export PS1="\[\033[01;36m\][nmm-ocaml-dev-shell]\033[00m\] $PS1"
  '';
}
