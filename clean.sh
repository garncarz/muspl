#!/bin/bash

mkdir -p out

shopt -s nullglob
for file in *.ly *.mid* *.pdf *.wav *.ogg *.mp3;
do
	mv $file out/
done

