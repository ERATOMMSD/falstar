# Fast Falsification with Adaptive Inputs

Contents

- [Quickstart](#quickstart)
- [ARCH 2018 friendly competition](#arch-2018-friendly-competition)

Requirements

- **Java 1.8**
- **Matlab (tested with R2017)**
- Scala 2.12 (build only)

## Quickstart:

Link the necessary MATLAB files and binaries

    ./link $MATLABROOT $ARCH

where `$MATLABROOT` points to the top-level of a specific Matlab installation
(e.g., ending in `../MATLAB/R2017b`)
and where `$ARCH` [depends on your OS](https://www.mathworks.com/help/matlab/matlab_external/setup-environment.html).
For 64 bit systems:

- Linux: `glnxa64`
- Mac OS X: `maci64`
- Windows: `win64`

There is a pre-built jar `main.jar`. Re-create with:

    make compile

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

## ARCH 2018 friendly competition

Re run

    make test

The benchmark models are in

    src/test/simulink

and the configuration scripts are in

    src/test/configuration

The results are in

    results/ARCH2018

as `csv` files following the naming scheme with team `MMSD`,
tool FALSTAR `FLST`, variant adaptive refinement `ar`.
Here's an example:

    model;property;algorithm;control points;exploration ratio;budget;seed;success;tries;min simulations;min time;max simulations;max time;avg simulations;avg time;best robustness
    fuel_control;□_[11.0, 50.0] abs(mu) < 0.04;adaptive;3 3 3 3 3;0.25;100;0;10;10;1;0;5;2;1;0;-0.006843342579650295

The fields are as follows:

- name of the Simulink model and property of interest
- algorithm and meta-parameters
- success rate and total number of trials
- aggregation over *successful* runs: number of simulations, run time per trial
  (min, max, average)
- best robustness achieved in any of the trials

Peak memory of the *JVM heap* is reported by the tool when re-running,
the Matlab memory is not measured here.
