#!/bin/sh

HERE=`dirname "$0"`
source $HERE/falstar-config.sh
scala -cp "$MATLABJARS:$HERE/falstar.jar" $@
