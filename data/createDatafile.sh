#!/bin/bash

cd `dirname $0`

awk '$1 != "#" {print substr($2, 3) "\t" substr($3, 3)}' JIS0208.TXT > KanjiOnly.txt
cat KanjiOnly.txt arib.txt > Kanji.txt

awk '/^3-/ && $2 != "#" {print substr($1,3) "\t" substr($2,3)}' jisx0213-2004-std.txt \
  | sed 's/+/	/g' > jisKanji-1.txt


awk '/^4-/ && $2 != "#" {print substr($1,3) "\t" substr($2,3)}' jisx0213-2004-std.txt \
  | sed 's/+/	/g' > jisKanji-2.txt
