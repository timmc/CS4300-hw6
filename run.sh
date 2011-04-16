#!/bin/bash
# Runs timmcHW6 program. See README for documentation.

set -o errexit
set -o nounset

cd "`dirname "$0"`"

if [ ! -f "timmcHW6-standalone.jar" ] ; then
  echo "Building..."
  ./build.sh
fi

java -jar timmcHW6-standalone.jar "$@" <&0
