#!/bin/sh
set -eu

gitbook="node_modules/gitbook-cli/bin/gitbook.js"

if ! test -e $gitbook; then
  npm install gitbook
  npm install gitbook-cli
fi

sbt 'project docs' tut && $gitbook build docs/target/tut docs/book

exit 0
