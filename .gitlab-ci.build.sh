#!/bin/bash
mkdir /tmp/myfiles
for i in {1..1000}; do
    dd if=/dev/random of=/tmp/myfiles/$i bs=1024 count=16 &
done
rm -r /tmp/myfiles
stack build
