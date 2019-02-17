# Fast Falsification with Adaptive Inputs

FalStar is a tool for the falsification of hybrid systems.
It takes as an input a model (currently: Simulink) and a configuration file
and tries to come up with an input signal such that the output signal produced by
the model falsifies given requirements.

Requirements

- Java 1.8, Scala 2.12 (compile time only)
- Matlab (tested with R2017b, R2018a)

Contact: gidonernst (*) gmail.com


## Quickstart

Determine MATLAB path

    ./falstar-config

Compile `falstar.jar` (not necessary)

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

## Repeatability

See respective output in `results/*/summary.csv`

[ARCH 2018 friendly competition](https://easychair.org/publications/paper/HjJ8)

    make arch2018
    
HSCC 2019

    make hscc2019
    

## Simulink model set up

Simulink models are accessed through their input and output ports
and through the Matlab function `sim`.
The model must currently be prepared for use as follows

- In the model configuration parameter setup, set the input to `[t__, u__]`.
  This will cause the simulation to read these two variables off the workspace
  and interpret them as the simulation input.
  `t__` is a column matrix of time points and `u__` provides the corresponding input vectors
  in each row for the respective time points (see an example above).
  The falsification tool provides these variables to the Matlab simulation engine in this form.
- The output signal should be recorded to `tout` and `yout` (which are the default settings).
- Make sure the output signal is not truncated
- Make sure that the *interpolation settings* on the input ports are as desired,
  for piecewise constant inputs disable interpolation.
  
## Command line options

    usage: falstar [-agv] file_1 ... file_n
      -a    ask for additional input files:
              enter one filename per line followed by a blank line
              a blank line acknowledges, EOF (CTRL+d) aborts
      -d    dummy run, parse and validate configuration only
      -g    show a graphical diagram for each trial
      -v    be verbose

## Configuration file syntax

The syntax is S-expression based.
Line comments are introduced with `;`.

FalStar maintains a context, consisting of

- a currently selected system on which it operates
- a currently selected falsification method

relative to which formulas are interpreted (e.g., parameter and input/output signal names).
Similar to SMT-LIB, contexts can be saved and restored with

    (push)
    ...
    (pop)

The configuration file can be composed of multiple parts, where `<path>` is a doubly-quoted path
that is interpreted relative to the working directory of FalStar (not relative to the including file):

    (include <path>)

System declarations (see below) and requirements for some models are in the `models` subfolder, ready to be used. 

### Selecting a system model

See `src/test/configuration/test.cfg` for an example.
Currently supported system types: `simulink`.

	(define-system <identifier>
	  	(simulink <path> [<load>])
	  	(parameters p1 ... pk)
	  	(inputs     i1 ... in)
	  	(outputs    o1 ... om)

	  	(constant   pi <num>)
	  	(constant   pi <num> <num>)
	  	
	  	(constant   ij <num>)
	  	(constant   ij <num> <num>)
	  	(piecewise-constant ij <num> <num>) 
	  	)

This declares the interface to a Simulink system, referred to by `<identifier>` subsequently.
The interface consists of

- Parameters `p1`, ..., `pk`, which are names of MATLAB variables that will be initialized by FalStar
- Inputs `i1`, ..., `in`, which give names to top-level input ports of the Simulink system (`In` blocks),
  in order of their numbers in Simulink (the names attached to the ports in the model are ignored)
- Outputs `o1`, ..., `om`, analogously for the output ports (`Out` blocks)

The following lines declare the ranges of parameters and inputs, one statement for each.

- Patamerets `pi` can be either a fixed constant value or a range, from which the value is chosen during falsification.
- Inputs `ii` can be either a fixed value, a constant input signal, or a piecewise constant input signal, again with a range of values in the latter two cases.
  Note that the time intervals for which these are held constant depends on the configuration of the falsification solver.

The current system can be selected by

    (select-system <identifier>
        <overrides>)

where `<identifier>` refers to a previously declared system,
and `<overrides>` are `(constant ...)` or `(piecewise-constant ...)` definitions
that can be used to override some of the settings from the system declaration.
This comes in handy if a system has several modes of operation (like the Powertrain model).

### Terms and Formulas

May refer to signal names (defined by the current model)

Arithmetic operators: `< > <= >= == != + - * / abs in`  
Logical connectives: `! => || && true false not implies and or always eventually`  
For example  

- `(abs (* 1.5 x))` denotes the absolute value of `1.5 x` where `x` is a signal name
- `(in x a b)` = `(and (<= a x) (<= x b))` asserts that `x` lies in the range `[a,b]`
- `(always (10 30) phi)` asserts that subformula `phi` holds over time interval `[10,30]`

### Falsification

First, a falsification method has to be selected.
Currently the following are supported

-   Random sampling `(set-solver random <cp> <max-iter>)`, where `<cp>` denotes the number of control points/segments of piecewise constant input signals and and `<max-iter>` is the upper bound of iterations/simulations to run

-   Adaptive probabilistic search `(set-solver adaptive (<cp1> <cp2> ...) <exploration> <max-iter>)`, where `<cp1>`, `<cp2>`, ... declares an (increasing) sequence of *granularity levels* with the corresponding number of control points, `<exploration>` is the exploration ratio (a good value is `0.25`) and `<max-iter>` is the maximum number of iterations

-   Breach `(<set-solver breach <cp> <opt> <max-iter>)` selects falsification by Breach with `<cp>` control points, optimization method `<opt>` (from `cmaes`, `global_nelder_mead`, ...)

Requirements can be falsified by

    (falsify <formula1> <formula2> ...)

Results are logged to in `csv` format if a log file is specified (in double quotes, relative to the working directory of FalStar)

    (set-log <path>)

Usually, the results are collected and merged, and only written out when FalStar terminates (or when another log file is specified).
The reason is that different solvers have different parameters that show up in the resulting output as extra columns.
Deferring to write the log means that these can be collected into one big table with a uniform schema.
Long running experiments might want to flush the log in between with `(flush-log)`.

The initial random seed can be specified by

    (set-seed <number>)

To request each falsification trial `(falsify ... )` to be repeated multiple times (with different seeds)

    (set-repeat <num>) 

### Definitions

Abbreviations, which are really just macros, can be defined as

    (define <id> <S-Expr>)

These are expanded immediately when encountered in certain places,
but interpreted only alongside the surrounding context.
Definitions are expdaned in formulas and almost in all places where numbers are expected.
