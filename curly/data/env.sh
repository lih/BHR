#!/bin/sh
install_curly() {
    curly --goody install.sh | sh -s "$@"
    case "$1" in
	bash-completions)
	    source "$HOME/.local/share/bashcomps/bashcomps.shl"
	    ;;
    esac
}
