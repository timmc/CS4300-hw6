#!/bin/bash
# Runs timmcHW5 program. See README for documentation.
# First run installs build manager into ~/.lein

set -o errexit
set -o nounset

cd "`dirname "$0"`"

LEIN_LOCAL="lein.sh"
LEIN_STABLE="https://github.com/technomancy/leiningen/raw/stable/bin/lein"

function first-run {
  echo "Installing Leiningen build tool into ~/.lein"
  curl --silent "$LEIN_STABLE" > "$LEIN_LOCAL"
  chmod +x "$LEIN_LOCAL"
  "./$LEIN_LOCAL" uberjar
}

if [ ! -f "$LEIN_LOCAL" ] ; then
  first-run
fi

java -jar timmcHW6-standalone.jar "$@"

