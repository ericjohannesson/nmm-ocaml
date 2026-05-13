SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -O globstar -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.PHONY: default clean test

default:
	make debian_package

clean:
	# remove anything in .gitignore, including directories
	git clean -fdX

package = nmm-ocaml-extras
version = 1.0
release = 1
architecture = amd64
maintainer = Eric Johannesson <eric@ericjohannesson.com>
homepage = https://github.com/ericjohannesson/nmm-ocaml
description = Some convenient wrapper scripts for nmm-ocaml (txt-of-nmm, html-of-nmm, pdf-of-nmm)
depends = nmm-ocaml, bash, sed, weasyprint

directory = ${package}_${version}-${release}_${architecture}

define control
Package: ${package}
Version: ${version}
Maintainer: ${maintainer}
Depends: ${depends}
Architecture: ${architecture}
Homepage: ${homepage}
Description: ${description}
endef

${directory}/DEBIAN/control:
	mkdir -p ${directory}/DEBIAN
	echo "${control}" > ${directory}/DEBIAN/control

${directory}/usr/bin/txt-of-nmm: ../../bin/txt-of-nmm.sh
	mkdir -p ${directory}/usr/bin
	cp ../../bin/txt-of-nmm.sh ${directory}/usr/bin/txt-of-nmm

${directory}/usr/bin/html-of-nmm: ../../bin/html-of-nmm.sh
	mkdir -p ${directory}/usr/bin
	cp ../../bin/html-of-nmm.sh ${directory}/usr/bin/html-of-nmm

${directory}/usr/bin/pdf-of-nmm: ../../bin/pdf-of-nmm.sh
	mkdir -p ${directory}/usr/bin
	cp ../../bin/pdf-of-nmm.sh ${directory}/usr/bin/pdf-of-nmm

${directory}/usr/share/bash-completion/completions/nmm-ocaml: ../../bin/nmm-ocaml-bash-completion.sh
	mkdir -p ${directory}/usr/share/bash-completion/completions
	cp ../../bin/nmm-ocaml-bash-completion.sh ${directory}/usr/share/bash-completion/completions/nmm-ocaml

${directory}/usr/bin: ${directory}/usr/bin/txt-of-nmm ${directory}/usr/bin/html-of-nmm ${directory}/usr/bin/pdf-of-nmm

${directory}: ${directory}/DEBIAN/control ${directory}/usr/bin ${directory}/usr/share/bash-completion/completions/nmm-ocaml

debian_package: ${directory}
	dpkg-deb --root-owner-group --build ${directory}
	mkdir -p debian_package
	mv *.deb debian_package/
