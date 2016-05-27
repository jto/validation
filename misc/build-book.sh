#!/bin/bash
set -eux

gitbook="node_modules/gitbook-cli/bin/gitbook.js"

if ! test -e $gitbook; then
  npm install gitbook
  npm install gitbook-cli
fi

sbt tut

(
  cd play-scalajs-example
  sbt js/fullOptJS
)

$gitbook build docs/tut docs/book

cp play-scalajs-example/js/target/scala-2.11/js-opt.js docs/book
cp play-scalajs-example/js/target/scala-2.11/js-launcher.js docs/book

exit 0
