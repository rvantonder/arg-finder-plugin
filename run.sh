#!/bin/sh

echo "Running plugin on $1..."
bap $1 --use-ida idaq --no-byteweight -larg_finder --arg_finder-outfile=out.txt
