#!/bin/sh
get_data() {
    printf "Installing goody $2 at location $1"
    curly --goody="$2" > "$1"
}
case "$1" in
    emacs)
	ver=`curly -v | cut -d' ' -f3`
	ver="${ver%.}"
	echo "Creating Emacs Lisp package: curly-$ver.tar"
	if [ ! -e "curly-$ver" ]; then
	    mkdir "curly-$ver"
	    get_data "curly-$ver/curly-mode.el" emacs/curly-mode.el
	    get_data "curly-$ver/curly-conf-mode.el" emacs/curly-conf-mode.el
	    get_data "curly-$ver/curly-utils.el" emacs/curly-utils.el
	    cat >"curly-$ver/curly-pkg.el" <<EOF
(define-package
  "curly"
  "$ver"
  "A major mode for Curly")
EOF
	    tar -cf "curly-$ver.tar" "curly-$ver"
	    rm -r "curly-$ver"
	fi
	;;

    bash-completions)
	data="${XDG_DATA_HOME:-$HOME/.local/share}/bashcomps/"
	mkdir -p "$data"
	get_data "$data/bashcomps.shl"			bash/completions/bashcomps.shl
	get_data "$data/completions/curly"		bash/completions/curly
	get_data "$data/completions/curly.arg.shf"	bash/completions/curly.arg.shf
	get_data "$data/completions/curly.script.shf"	bash/completions/curly.script.shf
	get_data "$data/completions/curly.sh"	        bash/completions/curly.sh
	;;
esac
