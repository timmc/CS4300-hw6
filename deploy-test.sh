#!/bin/bash

set -o errexit
set -o nounset

cd "`dirname "$0"`"

./mccormack_t_HW6/run.sh -f ./res/xmaskaos.rts -rf 95
