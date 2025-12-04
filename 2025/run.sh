#!/bin/sh
#

usage() {
	echo "$0 <d1|d2|d3...> [options]"
	exit 0
}

[ -e "$1.scm" ] || usage

guile -L . $1.scm $@
