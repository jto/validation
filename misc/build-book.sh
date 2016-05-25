#!/bin/sh
set -eu

gitbook="node_modules/gitbook-cli/bin/gitbook.js"

if ! test -e $gitbook; then
  npm install gitbook
  npm install gitbook-cli
fi

$gitbook build docs/tut docs/book

exit 0
