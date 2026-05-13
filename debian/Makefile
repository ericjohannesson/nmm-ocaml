SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -O globstar -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.PHONY: default clean test

default:
	make debian_packages

clean:
	# remove anything in .gitignore, including directories
	git clean -fdX



debian_packages: nmm-ocaml/debian_package nmm-ocaml-extras/debian_package
	mkdir -p debian_packages
	mv nmm-ocaml/debian_package/*.deb debian_packages/
	mv nmm-ocaml-extras/debian_package/*.deb debian_packages/

nmm-ocaml/debian_package: nmm-ocaml ../bin/nmm-ocaml ../bin/nmm-ocaml-bash-completion.sh
	cd nmm-ocaml
	make
	cd -

nmm-ocaml-extras/debian_package: nmm-ocaml-extras ../bin/txt-of-nmm.sh ../bin/html-of-nmm.sh ../bin/pdf-of-nmm.sh ../bin/nmm-ocaml-bash-completion.sh
	cd nmm-ocaml-extras
	make
	cd -


