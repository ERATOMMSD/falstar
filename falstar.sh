#!/bin/bash

HERE=`dirname "$0"`
source $HERE/falstar-config.sh
java -cp "$MATLABJARS:$HERE/falstar.jar" falstar.Main $@
