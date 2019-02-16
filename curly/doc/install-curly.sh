#!/bin/sh
curly_version="0.59.4.3"
curly_url="https://www.curly-lang.org/pkg/curly-$curly_version.tar.xz"

import_stdkeys=
prefix_dir="$HOME/.local"
lib_dir=
bin_dir=

while [ "$#" -gt  0 ]; do
    o="$1"; shift
    optval=
    case "$o" in
	-[pB]?*) optval="${o#-?}" optname="${o%$optval}";;
	--*=*) optname="${o%%=*}"; optval="${o#--*=}";;
	-[pB]|--prefix|--bin-dir) optname="$o"; optval="$1"; shift;;
	*) optname="$o";;
    esac
    case "$optname" in
	--import-standard-keys) import_stdkeys=true;;
	-p|--prefix) prefix_dir="$optval";;
	-L|--lib-dir) lib_dir="$optval";;
	-B|--bin-dir) bin_dir="$optval";;
    esac
done

if [ "${lib_dir:+x}" = '' ]; then
    read -p "Please enter a directory in which to install Curly (default: $prefix_dir/lib) : " lib_dir </dev/tty
fi
if [ "${lib_dir:+x}" = '' ]; then
    lib_dir="$prefix_dir/lib"
fi

if [ "${bin_dir:+x}" = '' ]; then
    bin_dir="$prefix_dir/bin"
fi

trace() { printf "\033[1m$ %s \033[m" "$*" >&2; "$@"; ret="$?"; echo >&2; return $ret; }
has_cmd() { which "$1" >/dev/null 2>&1; }
if has_cmd curl; then
    get_url() { curl -s "$1"; }
elif has_cmd wget; then
    get_url() { wget -q -O- "$1"; }
else
    get_url() { exit 1; }
fi

get_url "$curly_url" | { mkdir -p "$lib_dir"; trace tar -xJ --checkpoint=40 --checkpoint-action=dot -C "$lib_dir"; }
mkdir -p "$bin_dir"
trace ln -fs "$lib_dir/curly-$curly_version/curly" "$bin_dir/curly"
if [ -n "$import_stdkeys" ]; then
    "$bin_dir/curly" %'key import curly-std standard.curly-lang.org'
fi
