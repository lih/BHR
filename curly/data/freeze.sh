#!/bin/bash
curly -h "$(which $1 2>/dev/null || echo "$1")" "${@:2}" | {
    state=start
    while IFS= read line; do
	# echo "line: $state '$line'" >&2
	case "$state","$line" in
	    start,"Mounts:"*) printf "\n# Mounts \n\n"; state=mounts;;
	    mounts,"  * "*)
		mountopt="${line#  \* }"
		case "$mountopt" in
		    *=" resource"*) :;;
		    *=" source"*)
			mountroot="${mountopt%% = *}"
			cacheroot="${mountopt#*= source }"
			cacheroot="${cacheroot#* }"
			find "$cacheroot" -name '*.cyl' | xargs stat -c %N | while read file _ path; do
			    eval "file=$file; path=$path"
			    path="${path#${path%/*/*}}"
			    file="${file#$cacheroot}"
			    file="${file#/}"
			    file="${file%.cyl}"
			    path="${path%.cyl}"
			    printf "mount %s%s = library %s\n" "$mountroot${mountroot:+.}" "${file//\//.}" "${path//\//}"
			done
			;;
		    *) printf "mount %s\n" "$mountopt";;
		esac;;
	    mounts,"Targets:"*) printf "\n# Targets\n\n"; state=targets;;
	    targets,"  * "*)
		printf "target %s\n" "${line#  \* }";;
	esac
    done
}
