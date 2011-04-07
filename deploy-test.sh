#!/bin/bash

set -o errexit
set -o nounset

cd "`dirname "$0"`"

WEB_BASE="/course/cs4300/.www"

./mccormack_t_HW6/run.sh < "$WEB_BASE/HW6/TODO"
