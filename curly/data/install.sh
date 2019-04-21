#!/bin/sh
get_data() {
    printf "Installing goody %s at location %s\n" "$2" "$1"
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
	root="${XDG_DATA_HOME:-$HOME/.local/share}/bashcomps"
	mkdir -p "$root/completions"
	if [ ! -e "$root/bashcomps.shl" ]; then
	    get_data "$root/bashcomps.shl"			bash/completions/bashcomps.shl
	fi
	get_data "$root/completions/curly"		bash/completions/curly
	get_data "$root/completions/curly.arg.shf"	bash/completions/curly.arg.shf
	get_data "$root/completions/curly.script.shf"	bash/completions/curly.script.shf
	get_data "$root/completions/curly.sh"	        bash/completions/curly.sh
	;;

    handlers)
	root="${XDG_DATA_HOME:-$HOME/.local/share}/applications"
	mkdir -p "$root"
	
	cat > "$root/curly-uri.desktop" <<EOF
[Desktop Entry]
Version=1.0
Type=Application
Exec=/usr/bin/env curly-uri %U
Name=Install Curly Program
Comment=Installs a program from a Curly URI
Terminal=false
MimeType=x-scheme-handler/curly
EOF
	;;
esac
