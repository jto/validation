#!/bin/sh
set -eu

sbt tut

npm install gitbook
npm install gitbook-cli

node_modules/gitbook-cli/bin/gitbook.js \
  build \
  docs/tut \
  docs/book

exit 0
