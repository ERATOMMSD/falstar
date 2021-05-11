#!/bin/sh

# This script sets up the environment for FalStar
# 
# See also:
#     ./falstar
#     ./falstar-session

mkdir -p ~/.falstar

# The file ~/.falstar/matlab exports environment variable
# $MATLABROOT pointing to Matlab's base directory.
# It is generated on-demand as follows:
if [ ! -f ~/.falstar/matlab ]
then
    echo "Looking for Matlab installation..."
    matlab -nodesktop -nojvm -r "matlabconfig('$HOME/.falstar/matlab')"
fi

if [ -f ~/.falstar/matlab ]
then
# Bring $MATLABROOT into the environment
    source ~/.falstar/matlab
    echo "Matlab at: $MATLABROOT"
    export LD_LIBRARY_PATH="$MATLABROOT/bin/glnxa64:$MATLABROOT/sys/os/glnxa64"
    MATLABJARS="$MATLABROOT/java/jar/mvm.jar:$MATLABROOT/java/jar/javaenginecore.jar:$MATLABROOT/java/jar/matlab.jar:$MATLABROOT/java/jar/engine.jar:$MATLABROOT/java/jar/util.jar:$MATLABROOT/java/jar/capabilities.jar"
else
    echo "Could not determine Matlab installation."
fi

