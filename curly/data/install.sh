#!/bin/sh
get_data() {
    printf "Installing goody %s at location %s\n" "$2" "$1"
    curly --goody="$2" > "$1"
}
trace() {
    printf "Running: %s\n" "$*" >&2
    "$@"
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
	root="${XDG_DATA_HOME:-$HOME/.local/share}"
	mkdir -p "$root"

	echo "Installing desktop file at $root/applications/curly-uri.desktop" >&2
	cat > "$root/applications/curly-uri.desktop" <<EOF
[Desktop Entry]
Version=1.0
Type=Application
Exec=$root/curly/handlers/curly-uri %u
Name=Install Curly Program
Comment=Installs a program from a Curly URI
MimeType=x-scheme-handler/curly
EOF

	echo "Updating desktop database" >&2
	update-desktop-database "$root/applications"

	echo "Installing handler $root/curly/handlers/curly-uri" >&2
	mkdir -p "$root/curly/handlers"
	
	cat > "$root/curly/handlers/curly-uri" <<EOF
#!/bin/sh
curly --goody install.sh | sh -s - "\$@"
EOF
	chmod +x "$root/curly/handlers/curly-uri"

	;;

    curly:*)
	uri="${1#curly:}"
	lib="${uri#//*/}"
	host="${uri%$lib}"
	lib="${lib%%/*}"
	prog="${uri#$host$lib}"
	prog="${prog#/}"
	contains() {
	    case "$1" in
		*"$2"*) return 0;;
		*)   return 1;;
	    esac
	}
	while contains "$prog" /; do
	    prog="${prog%%/*}.${prog#*/}"
	done
	case "$host" in
	    //*/)
		host="${host#//}"; host="${host%/}"
		if [ -z "`curly %"key meta $host"`" ]; then
		    curly %"key import $host $host" 2>/dev/null
		fi
		cmd="curly --mount p=package:$host:$lib %'run p.$prog'"
		;;
	    *)
		cmd="curly --mount p=library:$lib %'run p.$prog'"
		;;
	esac
	if [ -t 1 ]; then
	    eval "$cmd"
	else
	    cache="${XDG_CACHE_HOME:-$HOME/.cache}/curly/logs"
	    mkdir -p "$cache"
	    ts=`date +%s,%F,%T`
	    (
		exec 2>&1
		cat > "$cache/cmd-$ts.log.html" <<EOF
<!DOCTYPE html>
<html>
  <head></head>
  <body>
    <pre style="background:black; color:white;"><span style="font-weight: bold">\$ $cmd</span>
`eval "$cmd" 2>&1`</pre>
  </body>
</html>
EOF
	    )
	    xdg-open "$cache/cmd-$ts.log.html"
	fi
	;;
esac
