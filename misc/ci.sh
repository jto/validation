#!/bin/bash
set -eux

sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION"

test_cmd="$sbt_cmd clean test"

coverage="$sbt_cmd clean coverage validationJVM/test && sbt coverageReport && sbt coverageAggregate && sbt coveralls"

run_cmd="$coverage && $test_cmd"

eval $run_cmd