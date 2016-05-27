#!/bin/bash
set -eux

sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION"

test_cmd="$sbt_cmd clean test"

coverage="$sbt_cmd clean coverage validationJVM/test coverageReport && sbt coverageAggregate && bash <(curl -s https://codecov.io/bash)"

run_cmd="$coverage && $test_cmd"

eval $run_cmd