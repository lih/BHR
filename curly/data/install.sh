#!/bin/sh
get_data() {
    curly --dump-data-file="$2" > "$1"
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
	    cat >"curly-$ver/curly-pkg.el" <<EOF
(define-package
  "curly"
  "$ver"
  "A package help in writing Curly code.")
EOF
	    tar -cf "curly-$ver.tar" "curly-$ver"
	    rm -r "curly-$ver"
	fi
	;;
esac
