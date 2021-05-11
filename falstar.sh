#!/bin/sh

HERE=`dirname "$0"`

source $HERE/falstar-config.sh

export LD_LIBRARY_PATH="$MATLABROOT/bin/glnxa64:$MATLABROOT/sys/os/glnxa64"

CP="$MATLABROOT/java/jar/mvm.jar:$MATLABROOT/java/jar/javaenginecore.jar:$MATLABROOT/java/jar/matlab.jar:$MATLABROOT/java/jar/engine.jar:$MATLABROOT/java/jar/util.jar:$MATLABROOT/java/jar/capabilities.jar"

java -cp "$CP:$HERE/falstar.jar" falstar.Main $@
