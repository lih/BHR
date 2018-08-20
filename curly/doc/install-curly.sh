#!/bin/sh
curly_version="0.59.4"
curly_url="https://www.curly-lang.org/pkg/curly-$curly_version.tar.xz"

import_stdkeys=
install_dir=
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
	-p|--prefix) install_dir="$optval";;
	-B|--bin-dir) bin_dir="$optval";;
    esac
done

if [ "${install_dir:+x}" = '' ]; then
    read -p "Please enter a directory in which to install Curly (default: $HOME/.local/lib) : " install_dir </dev/tty
fi
if [ "${install_dir:+x}" = '' ]; then
    install_dir="$HOME/.local/lib"
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

get_url "$curly_url" | { mkdir -p "$install_dir"; trace tar -xJ --checkpoint=40 --checkpoint-action=dot -C "$install_dir"; }
if [ -n "$bin_dir" ]; then
    mkdir -p "$bin_dir"
    trace ln -fs "$install_dir/curly-$curly_version/curly" "$bin_dir/curly"
fi
if [ -n "$import_stdkeys" ]; then
    "$bin_dir/curly" %'key import curly-std standard.curly-lang.org' %'key set curly-std follow-branches = stdlib hello'
fi
