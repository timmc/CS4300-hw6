#!/bin/bash
# Builds timmcHW6 program. See README for documentation.

set -o errexit
set -o nounset

cd "`dirname "$0"`"

LEIN_LOCAL="lein.sh"
LEIN_STABLE="https://github.com/technomancy/leiningen/raw/stable/bin/lein"

function first-run {
  echo "Installing Leiningen build tool into ~/.lein"
  curl --silent "$LEIN_STABLE" > "$LEIN_LOCAL"
  chmod +x "$LEIN_LOCAL"
}

if [ ! -f "$LEIN_LOCAL" ] ; then
  first-run
fi

"./$LEIN_LOCAL" uberjar
