package="nmm-ocaml-extras"
version="1.0"
release="1"
architecture="amd64"
maintainer="Eric Johannesson <eric@ericjohannesson.com>"
homepage="https://github.com/ericjohannesson/nmm-ocaml"
description="Some convenient wrapper scripts for nmm-ocaml (txt-of-nmm, html-of-nmm, pdf-of-nmm)"
depends="nmm-ocaml, bash, sed, weasyprint"

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

cp ../bin/txt-of-nmm.sh ${directory}/usr/bin/txt-of-nmm
chmod +x ${directory}/usr/bin/txt-of-nmm

cp ../bin/html-of-nmm.sh ${directory}/usr/bin/html-of-nmm
chmod +x ${directory}/usr/bin/html-of-nmm

cp ../bin/pdf-of-nmm.sh ${directory}/usr/bin/pdf-of-nmm
chmod +x ${directory}/usr/bin/pdf-of-nmm

dpkg-deb --root-owner-group --build $directory

