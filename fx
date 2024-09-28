#!/bin/bash
./f | fgrep -v '.#' | sort | tr \\n \\0 | xargs -0 "$@" || exit $?
