#!/bin/sh
curly_url="http://www.curly-lang.org/pkg/curly.tar.xz"

while (($# > 0)); do
    cur_opt="$1"
    shift
    case "$cur_opt" in
	--prefix)  bin_dir="$1/bin"; install_dir="$1/lib"; shift;;
	--bin-dir) bin_dir="$1"; shift;;
	--lib-dir) install_dir="$1"; shift;;
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
    trace ln -fs "$install_dir/curly-0.59.4/curly" "$bin_dir/curly"
fi
