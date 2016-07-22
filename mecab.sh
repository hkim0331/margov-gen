#!/bin/sh
# use ipadic
#
# * 2016-07-22 now calling the bellow command from trivial-shell directly,
#   so, this shell script is no use.
echo $1 | mecab --output-format-type=wakati

