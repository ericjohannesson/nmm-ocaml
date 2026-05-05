package="nmm-ocaml"
version="1.0"
release="1"
architecture="amd64"
maintainer="Eric Johannesson <eric@ericjohannesson.com>"
homepage="https://github.com/ericjohannesson/nmm-ocaml"
description="An implementation of no-markup-markup in OCaml"
depends="libc6"

directory="${package}_${version}-${release}_${architecture}"

mkdir -p ${directory}/DEBIAN

echo "Package: ${package}
Version: ${version}
Maintainer: ${maintainer}
Depends: ${depends}
Architecture: ${architecture}
Homepage: ${homepage}
Description: ${description}" > ${directory}/DEBIAN/control

mkdir -p ${directory}/usr/bin
cp ../bin/nmm-ocaml ${directory}/usr/bin/

mkdir -p ${directory}/usr/share/bash-completion/completions
cp ../bin/nmm-ocaml-bash-completion.sh ${directory}/usr/share/bash-completion/completions/nmm-ocaml

dpkg-deb --root-owner-group --build $directory 
