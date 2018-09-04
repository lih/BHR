#!/bin/sh
set -ue

if which dig >/dev/null 2>&1;
then lookup() { dig +noall +answer "$1" "$2"; }
elif which host >/dev/null 2>&1
then lookup() { host -t SRV "$1"; }
elif which drill >/dev/null 2>&1
then
    lookup() {
	drill "$2" "$1" | while IFS= read line; do
	    case "$line" in
		';;'*) :;;
		*"$1"*) printf "%s\n" "$line";;
	    esac
	done
    }
fi

case "$1" in
    domain-vc)
	domain="$(hostname -d)"
	lookup SRV _curly-vc._tcp.$domain | while read _ _ _ _ _ _ port srv; do
	    printf '("%s",%s)\n' "${srv%.}" "$port"
	done
	;;
    domain-key)
	domain="$2"
	lookup TXT "$domain" | sort | {
	    total=""
	    while read _ _ _ _ val; do
		case "$val" in
		    "\"curly-id-"*"="*) val="${val#\"curly-id-*=}"
					total+="${val%\"}"
					;;
		esac
	    done
	    printf "%s\n" "$total"
	}
	;;
esac
