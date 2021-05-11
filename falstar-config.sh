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
else
    echo "Could not determine Matlab installation."
fi
