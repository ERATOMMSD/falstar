# Fast Falsification with Adaptive Inputs

Quickstart:

Link the necessary MATLAB files and binaries

    ./link $MATLABROOT $ARCH

where `$MATLABROOT` points to the top-level of a specific Matlab installation
(e.g., ending in `../MATLAB/R2017b`)
and where `$ARCH` [depends on your OS](https://www.mathworks.com/help/matlab/matlab_external/setup-environment.html).
For 64 bit systems:

- Linux: `glnxa64`
- Mac OS X: `maci64`
- Windows: `win64`

Try it out

    ./run -g src/test/configuration/test.cfg

falsifies a Simulink model of an automatic transmission against specification
`□_[0,30] speed < 120` and displays various input and output signals.

Here is an example output:

	trial 1/1
	property □_[0.0, 30.0] speed < 120.0
	algorithm adaptive
	  control points: 2 2 3 3 3 4
	  exploration ratio: 0.25
	  budget: 100
	starting matlab ... connected (4s)
	initializing 'automatic_transmission' ... done (17s)
	......
	
	inputs
	  t__ = [0.0; 10.0; 25.0; 30.0]
	  u__ = [75.0 0.0; 100.0 0.0; 37.5 243.75; 37.5 243.75]
	falsified with robustness -6.849703872580591
	
	statistics
	  simulations 6
	  total time  29s
	  peak memory 37111 kb
	
	bye
