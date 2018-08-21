#!/bin/bash
stack_path="$(stack path --local-install-root)"

for exe; do
    version="$(sed -n 's/^version: *//p' "$exe/$exe.cabal")"
    full="$exe-$version"
    shopt -s nullglob
    (
	tmp="$(mktemp -d)"
	mkdir -p "$tmp/$full"
	cd "$stack_path"
	cp -r bin/$exe share/*/$exe-*/* "$tmp/$full"
	tar -C "$tmp" -c .
	rm -r "$tmp"
    ) | xz > "$exe-$TRAVIS_OS_NAME.tar.xz"
done
