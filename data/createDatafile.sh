#!/bin/bash

cd `dirname $0`

awk '/^3-/ && $2 != "#" {print substr($1,3,length($1)), substr($2,3,length($2))}' jisx0213-2004-std.txt \
  | sed 's/+/ /g' > jisx0213-2004-std-1.txt


awk '/^4-/ && $2 != "#" {print substr($1,3,length($1)), substr($2,3,length($2))}' jisx0213-2004-std.txt \
  | sed 's/+/ /g' > jisx0213-2004-std-2.txt
