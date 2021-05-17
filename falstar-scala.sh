#!/bin/sh

HERE=`dirname "$0"`
source $HERE/falstar-config.sh
java -cp "$MATLABJARS:$HERE/falstar.jar" scala.tools.nsc.MainGenericRunner $@
