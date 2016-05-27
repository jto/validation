#!/bin/bash
set -eux

sbt ++$TRAVIS_SCALA_VERSION clean test tut
