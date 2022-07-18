# Fast Falsification with Adaptive Inputs

FalStar is a tool for the falsification of hybrid systems.
It takes as an input a model (currently: Simulink) and a configuration file
and tries to come up with an input signal such that the output signal produced by
the model falsifies given requirements.

Requirements

- Java 1.8, Scala 2.12 (compile time only)
- Matlab (tested different versions after R2017)

Contact: gidonernst (*) gmail.com

- Gidon Ernst, Sean Sedwards, Zhenya Zhang, Ichiro Hasuo:
  *Fast Falsification of Hybrid Systems using Probabilistically Adaptive Input*,
  QEST 2019, Preprint: <https://arxiv.org/abs/1812.04159>

- Gidon Ernst, Sean Sedwards, Zhenya Zhang, and Ichiro Hasuo:
  *Falsification of hybrid systems using adaptive probabilistic search*,
  Transactions on Modeling and Computer Simulations, 2021

## Quickstart

Determine MATLAB path

    ./falstar-config.sh

Compile `falstar.jar`

    make compile
    
Test whether everything worked

    ./falstar.sh # prints usage instructions
    make test # run a simple test case

This falsifies a Simulink model of an automatic transmission against specification
`always_[0,30] speed < 120` and displays various input and output signals. Here's the output:

	trial 1/1
	property always_[0.0, 30.0] speed < 120.0
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

    ./falstar-session.sh
    

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

    usage: falstar [-dv] file_1 ... file_n
      -d    dummy run, parse and validate configuration only
      -v    be more verbose

In particular, the `-v` flag shows all interaction with the Matlab engine.

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
that is interpreted relative to the working directory of FalStar:

    (include <path>)

The path is relatve to the *inluding* file, except when it starts with '.' or with '/' then it is used as given.

System declarations (see below) and requirements for some models are in the `models` subfolder, ready to be used. 

### Selecting a system model

See `src/resource/configuration/test.cfg` for an example.
Currently supported system types: `simulink` and `matlab`.

	(define-system <identifier>
	  	
        ; two alternative ways to set up systems
        (simulink <path> [<load>])
        (matlab   <name> <path> <init> <run>)

        ; common configuration options
	  	(parameters p1 ... pk)
	  	(inputs     i1 ... in)
	  	(outputs    o1 ... om)

        ; valid input ranges
	  	(constant   pi <num>)
	  	(constant   pi <num> <num>)
	  	
	  	(constant   ij <num>)
	  	(constant   ij <num> <num>)
	  	(piecewise-constant ij <num> <num>) 
	  	)

A system definition comprises of a declaration of how the simulation is executed,
by a `simulink` or `matlab` clause,
followed by a declaration of the system's interface in terms of parameters, input, and output ports;
and a declaration of valid ranges reps. values for these which can later be overriden if desired

#### System Type: `simulink`

A simulink system is declared by
 
    (simulink <path> [<load>])

where `<path>` must point to a `.mdl` or `.slx` file.
An optional list `<load>` of `.m` files or `.mdl` files can be specified.
The initialization sequence runs

    addpath(<dir>)        % where <dir> is the directory containing <path>
    load_system(<name>)   % where <name> is the file name stem (without suffix)

    <load>                % execute .m files without suffix
    load(<load>)          % load .mat files

and each simulation is executed with the `sim` function,
where the stopping time and the external inputs are explicitly given by Falstar
(i.e., those defined in the model are overriden).

#### System Type: Matlab

A system scripted in Matlab is declared by

    (matlab <name> <path> <init> <run>)

where `<name>` is an arbitrary identifier (implicit for Simulink systems from the file name),
`<path>` is the base path where scripts are located, and `<init>` and `<run>` are two Matlab functions used to initialize the system
and run simulations respectively.

Falstar executes initially

    addpath(<path>)
    <init>

and for each simulation

    <run>(p, u, T)

where `p` is an array of values for the parameters, `u` is a time-varying input signal,
and `T` is the stopping time.
The expected result is a pair `[tout, yout]` describing the output trace,
in the same format as `.tout` and `.yout` from the `Array` format specified for Simulink systems.

#### Interface Description

The interface description consists of

- Parameters `p1`, ..., `pk`, which are names of MATLAB variables that will be initialized by FalStar
- Inputs `i1`, ..., `in`, which give names to top-level input ports of the Simulink system (`In` blocks),
  in order of their numbers in Simulink (the names attached to the ports in the model are ignored)
- Outputs `o1`, ..., `om`, analogously for the output ports (`Out` blocks)

#### Definining Input Ranges

The following lines declare the ranges of parameters and inputs, one statement for each.

- Patamerets `pi` can be either a fixed constant value or a range, from which the value is chosen during falsification.
- Inputs `ii` can be either a fixed value, a constant input signal, or a piecewise constant input signal, again with a range of values in the latter two cases.
  Note that the time intervals for which these are held constant depends on the configuration of the falsification solver.

#### Selecting a System

More than one system can be defined within a Falstar script.
You can pick the system that is going to be used for the subsequent falsification attempts or for validation:

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

### Definitions

Abbreviations, which are really just macros, can be defined as

    (define <id> <S-Expr>)

These are expanded immediately when encountered in certain places,
but interpreted only alongside the surrounding context.
Definitions are expanded in formulas and almost in all places where numbers are expected.

### Falsification

First, a falsification method has to be selected.
Currently the following are supported

-   Random sampling `(set-solver random <cp> <max-iter>)`, where `<cp>` denotes the number of control points/segments of piecewise constant input signals and and `<max-iter>` is the upper bound of iterations/simulations to run

-   Adaptive probabilistic search `(set-solver adaptive (<cp1> <cp2> ...) <exploration> <max-iter>)`, where `<cp1>`, `<cp2>`, ... declares an (increasing) sequence of *granularity levels* with the corresponding number of control points, `<exploration>` is the exploration ratio (a good value is `0.25`) and `<max-iter>` is the maximum number of iterations

Moreover, the following bridge might still work (untested):

-   Breach `(<set-solver breach <cp> <opt> <max-iter>)` selects falsification by Breach with `<cp>` control points, optimization method `<opt>` (from `cmaes`, `global_nelder_mead`, ...)

Requirements can be falsified by

    (falsify <formula1> <formula2> ...)

Results are logged to in `.csv` format if specified (in double quotes, relative to the current file FalStar).
The log file collects entries for each individual falsification trial, whereas the report groups them together
(cf. `(set-repeat _)` below) with aggregated information on number of simulations, running time, and success rate.

    (set-log    <path>)
    (set-report <path>)

Usually, log and report are written only at the end, but they can be flushed eagerly with `(flush-log)`,
which reads in any previous values stored in that file and integrates the new results.
It is important to note that different solvers produce different columns in the table, such that this merging is necessary.

The initial random seed can be specified by

    (set-seed <number>)

To request each falsification trial `(falsify ... )` to be repeated multiple times (with different seeds)

    (set-repeat <num>)

### Validation Scripts

The general procedure for validation is described separately in file `Validation.md`.
In order to make use of this feature, one has to provide particular scripts as follows.
First, set up where the results should be stored, in a similar format to falsification runs.
Both files will be in `.csv` format, existing files with results are extended.

    (set-log    <path>)
    (set-report <path>)

In order to validate, one can give the path to a `.csv` file simply by

    (validate <path>)

This assumes that all models referred to by this `<path>` in terms of their mnenonic codes
have been set up properly in the script before this line.