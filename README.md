# Fast Falsification with Adaptive Inputs

FalStar is a tool for the falsification of hybrid systems.
It takes as an input a model (currently: Simulink) and a configuration file
and tries to come up with an input signal such that the output signal produced by
the model falsifies given requirements.

Requirements

- Java 1.8, Scala 2.12
- Matlab (tested with R2017b)

Contact: gidonernst (*) gmail.com

See also [ARCH 2018 friendly competition](https://cps-vo.org/group/ARCH/FriendlyCompetition)


## Quickstart

Link the necessary MATLAB files and binaries

    ./link $MATLABROOT $ARCH

where `$MATLABROOT` points to the top-level of a specific Matlab installation
(e.g., ending in `../MATLAB/R2017b`)
and where `$ARCH` [depends on your OS](https://www.mathworks.com/help/matlab/matlab_external/setup-environment.html).
For 64 bit systems:

- Linux: `glnxa64`
- Mac OS X: `maci64`
- Windows: `win64`

Compile `falstar.jar`

    make compile
    
Test whether everything worked

    ./falstar # prints usage instructions
    make test # run a simple test case

This falsifies a Simulink model of an automatic transmission against specification
`□_[0,30] speed < 120` and displays various input and output signals. Here's the output:

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

If you're tired of waiting for Matlab to initialize on every trial, you can keep an active instance running.
Be aware that this caches initialized models as well, so if you change those, you need to restart the session.
It's a simple command line interface to Matlab (currently without error handling, and it will terminate if you type in an invalid command).

    ./falstar-session

### Simulink model set up

Simulink models are accessed through their input and output ports
and through the Matlab function `sim`.
The model must be prepared for use as follows

- In the model configuration parameter setup, set the input to `[t__, u__]`.
  This will cause the simulation to read these two variables off the workspace
  and interpret them as the simulation input.
  `t__` is a column matrix of time points and `u__` provides the corresponding input vectors
  in each row for the respective time points (see an example above).
  The falsification tool provides these variables to the Matlab simulation engine in this form.
- The output format should be set to `Data structure with time`
  (although this will be selected automatically in the future).
  The output signal should be recorded to `tout` and `yout` (which are the default settings).
- Make sure the output signal is not truncated
- Make sure that the interpolation settings on the input ports are as desired,
  for piecewise constant inputs disable interpolation.

### Configuration file syntax


