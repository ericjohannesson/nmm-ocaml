{ pkgs ? import (
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/26.05.tar.gz";
      sha256 = "sha256-md0zn0RnwNvPyASas1yG5YUuwQ4ALA6ucL50l0DvqCo=";
    }
  ) { }
}:
pkgs.stdenv.mkDerivation {
  pname = "nmm-ocaml";
  version = "7";
  src = ./.;
  buildInputs = with pkgs; [
    ocaml
    ocamlPackages.findlib
    ocamlPackages.sedlex
    ocamlPackages.uuseg
    ocamlPackages.xml-light
  ];
  buildPhase = ''
    make bin/nmm-ocaml
    make share
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp bin/nmm-ocaml $out/bin/
    cp -r share $out/
  '';
}
