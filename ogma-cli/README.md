# OGMA

Ogma is a tool to facilitate the integration of safe runtime monitors into
other systems. Ogma extends
[Copilot](https://github.com/Copilot-Language/copilot), a high-level runtime
verification framework that generates hard real-time C99 code.

# Features

- Translating requirements defined in [NASA's requirements elicitation tool
  FRET](https://github.com/NASA-SW-VnV/fret) into corresponding monitors in
  Copilot.

- Generating [NASA Core Flight System](https://cfs.gsfc.nasa.gov/) applications
  that use Copilot for monitoring data received from the message bus.

- Generating message handlers for NASA Core Flight System applications to make
  external data in structs available to a Copilot monitor.

- Generating the glue code necessary to work with C structs in Copilot.

- Generating [Robot Operating System](https://ros.org) applications that use
  Copilot for monitoring data published in different topics.

- Generating [F' (FPrime)](https://github.com/nasa/fprime/) components that use
  Copilot for monitoring data published in different ports.


<p align="center">
  <img src="https://raw.githubusercontent.com/nasa/ogma/gh-pages/images/fret-to-c.gif" alt="Conversion of requirements into C code">
  <br />
  <i>Conversion of FRET requirements into C code.</i>
</p>

<p align="center">
  <img src="https://raw.githubusercontent.com/nasa/ogma/gh-pages/images/simulator.gif" alt="Monitoring within simulation video">
  <br />
  <i>Integration of monitors into larger applications (e.g., simulators).</i>
</p>

## Table of Contents

- [Installation](#installation)
  - [Pre-requisites](#pre-requisites)
  - [Compilation](#compilation)
- [Usage](#usage)
  - [Language Transformations: FRET](#language-transformations-fret)
  - [cFS Application Generation](#cfs-application-generation)
  - [Struct Interface Generation](#struct-interface-generation)
  - [ROS Application Generation](#ros-application-generation)
  - [F' Component Generation](#f-component-generation)
- [Contributions](#contributions)
- [Acknowledgements](#acknowledgements)
- [License](#license)


# Installation
<sup>[(Back to top)](#table-of-contents)</sup>

## Pre-requisites
<sup>[(Back to top)](#table-of-contents)</sup>

To install Ogma from source, users must have the tools GHC and cabal-install.
At this time, we recommend GHC 8.6 and a version of cabal-install between 2.4
and 3.2. (Ogma has been tested with GHC versions up to 9.2 and cabal-install
versions up to 3.6, although the installation steps may vary slightly depending
on the version of cabal-install being used.)

On Debian or Ubuntu Linux, both can be installed with:

```sh
$ apt-get install ghc cabal-install
```

On Mac, they can be installed with:

```sh
$ brew install ghc cabal-install
```

## Compilation
<sup>[(Back to top)](#table-of-contents)</sup>

Once GHC and cabal are installed, the simplest way to install Ogma is with:
```sh
$ git clone https://github.com/nasa/ogma.git
$ cd ogma
$ export PATH="$HOME/.cabal/bin/:$PATH"
$ cabal v1-install alex happy
$ cabal v1-install BNFC copilot
$ cabal v1-install ogma-*/
```

After that, the `ogma` executable will be placed in the directory
`$HOME/.cabal/bin/`, where `$HOME` represents your user's home directory.

# Usage
<sup>[(Back to top)](#table-of-contents)</sup>

The main invocation of `ogma` with `--help` lists sub-commands available:
```sh
$ ogma --help
ogma - an anything-to-Copilot application generator

Usage: ogma COMMAND
  Generate complete or partial Copilot applications from multiple languages

Available options:
  -h,--help                Show this help text

Available commands:
  structs                  Generate Copilot structs from C structs
  handlers                 Generate message handlers from C structs
  cfs                      Generate a complete cFS/Copilot application
  fprime                   Generate a complete F' monitoring component
  fret-component-spec      Generate a Copilot file from a FRET Component
                           Specification
  fret-reqs-db             Generate a Copilot file from a FRET Requirements
                           Database
  ros                      Generate a ROS 2 monitoring package
```

## Language transformations: FRET
<sup>[(Back to top)](#table-of-contents)</sup>

[FRET](https://github.com/NASA-SW-VnV/fret) is a requirements elicitation tool
created by NASA Ames Research Center. Requirements can be specified in
structured natural language called FRETish, and the tool helps users understand
them, validate them, and formalize them. For instructions on how to specify,
analyze and export FRET requirements, see [the FRET
manual](https://github.com/NASA-SW-VnV/fret/blob/master/fret-electron/docs/_media/userManual.md).

<p align="center">
<img src="https://raw.githubusercontent.com/nasa/ogma/develop/docs/fret.png" width="75%">
<br />
<sup><i>Screenshot of requirement specified inside NASA's requirements elicitation tool FRET.</i></sup>
</p>

Ogma can convert specifications generated by FRET into Copilot monitors.
Specifically, the commands `fret-component-spec` and `fret-reqs-db` allow users
to interact with the different kinds of files produced by FRET.

FRET files include properties encoded using Temporal Logic, both in
[SMV](http://www.cs.cmu.edu/~modelcheck/smv.html) and in
[CoCoSpec](https://link.springer.com/chapter/10.1007%2F978-3-319-41591-8_24),
the latter of which is an extension of Lustre. Ogma uses the SMV expressions by
default, but the CLI flag `--cocospec` can be used to select the CoCoSpec
variant of requirements instead.

As an example, from the following FRET requirement:
```
test_component shall satisfy (input_signal <= 5)
```

Ogma generates the following Copilot specification:

```haskell
import Copilot.Compile.C99
import Copilot.Language          hiding (prop)
import Copilot.Language.Prelude
import Copilot.Library.LTL       (next)
import Copilot.Library.MTL       hiding (since, alwaysBeen, trigger)
import Copilot.Library.PTLTL     (since, previous, alwaysBeen)
import Language.Copilot          (reify)
import Prelude                   hiding ((&&), (||), (++), not, (<=), (>=), (<), (>))

input_signal :: Stream Double
input_signal = extern "input_signal" Nothing

-- | propTestCopilot_001
--   @
--   test_component shall satisfy (input_signal <= 5)
--   @
propTestCopilot_001 :: Stream Bool
propTestCopilot_001 = ( alwaysBeen (( ( ( input_signal <= 5 ) ) )) )

-- | Complete specification. Calls the C function void  handler(); when
-- the property is violated.
spec :: Spec
spec = do
  trigger "handlerpropTestCopilot_001" (not propTestCopilot_001) []

main :: IO ()
main = reify spec >>= compile "fret"
```

This program can be compiled using Copilot to generate a `fret.c` file that
includes a hard real-time C99 implementation of the monitor. The specification
generated by FRET for the FRETish requirement shown above is included with the
Ogma distribution, and can be tested with:

```sh
$ ogma fret-component-spec --cocospec --fret-file-name examples/fret-reqs-small.json > FretCopilot.hs
$ runhaskell FretCopilot.hs
```

The first step executes `ogma`, generating a Copilot monitor in a file called
`FretCopilot.hs`. The second step executes the Copilot compiler, generating a C
implementation `fret.c` and C header file `fret.h`.

The resulting `fret.c` file can be tested with the main provided in
`examples/fret-reqs-small-main.c`, which defines a handler for Copilot to call
when the property monitored is violated, and a main function that steps through
the execution, providing new data for the Copilot monitor:

```c
#include <stdio.h>

double input_signal;  // Input data made available for the monitor
void step(void);      // Copilot monitor's main entry point

void handlerpropTestCopilot_001(void) {
  printf("Monitor condition violated\n");
}

int main (int argc, char** argv) {
  int i = 0;

  input_signal = 0;

  for (i=0; i<10; i++) {
    printf("Running step %d\n", i);
    input_signal += 1;
    step();
  }
  return 0;
}
```

To compile both files, run `gcc examples/fret-reqs-small-main.c fret.c -o
main`. Executing the resulting `main` shows that the condition is violated
after a number of steps:
```
Running step 0
Running step 1
Running step 2
Running step 3
Running step 4
Running step 5
Monitor condition violated
Running step 6
Monitor condition violated
Running step 7
Monitor condition violated
Running step 8
Monitor condition violated
Running step 9
Monitor condition violated
```

Take a peek inside the intermediate files `FretCopilot.hs`, `fret.c` and
`fret.h` to see what is being generated by Ogma and by Copilot.

The generated C code can be integrated as part of a larger application. For
example, the following shows a Copilot monitor generated from a FRET file
integrated in an X-Plane widget that presents information to users during a
flight in the X-Plane simulator.

<p align="center">
<img src="https://raw.githubusercontent.com/nasa/ogma/develop/docs/xplane.png" width="75%">
<br />
<sup><i>Screenshot of Copilot monitor generated by Ogma from FRET requirement,
integrated into the X-Plane flight simulator. The widget on the right side of
the screen presents information received and returned by the monitor, with a
red/fire icon to signal that the monitor has been triggered (i.e., that the
property has been violated).</i></sup>
</p>

**Numeric Representations**

FRET Component Specifications use the types `real` and `int` to represent
different numeric variables. Copilot distinguishes between different numeric
representations and supports multiple precisions, and so does the final C
code generated from the Copilot specification.

To help users generate code that works as part of a larger system without
modifications, Ogma includes two additional flags to map the types `real` and
`int` to specific Copilot (Haskell) types. For example, the following command
would generate a Copilot specification for a hypothetical
`numeric-example.json` FRET CS file while mapping all real variables to the
type `Double` and all integer variables to the type `Int32`:

```
$ ogma fret-component-spec --fret-file-name numeric-example.json --map-int-to Int32 --map-real-to Double
```

In the name of flexibility, Ogma does not sanitize the values of these
variables and copies the types passed to these options verbatim to the
generated Copilot code. It is the user's responsibility to ensure the types
passed are valid Haskell types within the scope of the module generated.
Note that Copilot supports only a limited subset of numeric types, which
must be instances of the type class
[`Typed`](https://hackage.haskell.org/package/copilot-core/docs/Copilot-Core-Type.html#t:Typed).

## cFS Application Generation

[NASA Core Flight System](https://cfs.gsfc.nasa.gov/) (cFS) is a flight
software architecture to implement complex systems by combining multiple
reusable applications that communicate to one another via a software bus. cFS
has been used, among others, on spacecraft, cubesats, and drones.

Ogma includes multiple facilities to generate cFS applications. The cFS
applications generated by Ogma perform three steps to connect Copilot monitors
to the application:
- Subscribe to a message in the cFS communication bus.
- When a message of the desired kind arrives, copy the data to make it
  available to Copilot and call the monitor's main entry point.
- Declare handlers that are executed when the property being monitored is
  violated.

When using this facility, Ogma produces a Copilot file that the user is
expected to modify to implement the property to monitor. To avoid having to
modify the generated C files that implement the cFS app itself, Ogma gives the
ability to state what information one is interested in monitoring. If the kind
of information is known to Ogma, it will automatically subscribe to the
necessary messages and make it available to Copilot. Ogma provides additional
flags to customize the list of known variables, so that projects can maintain
their own variable databases beyond what Ogma includes by default.

cFS applications are generated using the Ogma command `cfs`, which receives
three main arguments:
- `--app-target-dir DIR`: location where the cFS application files must be
  stored.
- `--variable-file FILENAME`: a file containing a list of variables that must
be made available to the monitor.
- `--variable-db FILENAME`: a file containing a database of known variables,
and the message they are included with.

The following execution generates an initial cFS application for runtime
monitoring using Copilot:
```
$ ogma cfs --variable-db examples/cfs-variable-db --variable-file examples/cfs-variables
```

The application generated by Ogma contains the following files:
```
copilot-cfs-demo/CMakeLists.txt
copilot-cfs-demo/fsw/for_build/Makefile
copilot-cfs-demo/fsw/mission_inc/copilot_cfs_perfids.h
copilot-cfs-demo/fsw/platform_inc/copilot_cfs_msgids.h
copilot-cfs-demo/fsw/src/copilot_cfs.c
copilot-cfs-demo/fsw/src/Properties.hs
copilot-cfs-demo/fsw/src/copilot_cfs_msg.h
copilot-cfs-demo/fsw/src/copilot_cfs_events.h
copilot-cfs-demo/fsw/src/copilot_cfs_version.h
copilot-cfs-demo/fsw/src/copilot_cfs.h
```

Users are expected to modify `Properties.hs` to adjust the property being
monitored. Although it is possible to adjust the file `copilot_cfs.c` to
include property violation handlers, we recommend adding them in a separate C
file and modifying the compilation scripts to include that additional file.
That way, invoking Ogma again will not overwrite the changes made to the cFS
application.

In this particular example, the C code generated contains the following
instruction to subscribe to an `ICAROUS_POSITION_MID` message to obtain
the vehicle position:
```c
    CFE_SB_Subscribe(ICAROUS_POSITION_MID, COPILOT_CommandPipe);
```

The message dispatcher included in the application detects a message
of this kind and calls a dedicated subroutine:
```c
void COPILOT_ProcessCommandPacket(void)
{
    CFE_SB_MsgId_t  MsgId;

    MsgId = CFE_SB_GetMsgId(COPILOTMsgPtr);

    switch (MsgId)
    {
        case ICAROUS_POSITION_MID:
            COPILOT_ProcessIcarousPosition();
            break;
    ...
```

Finally, the dedicated subroutine makes data available to the monitor
and calls the main Copilot entry point `step`:

```c
void COPILOT_ProcessIcarousPosition(void)
{
    position_t* msg;
    msg = (position_t*) COPILOTMsgPtr;
    position = *msg;
    step();
}
```

## ROS Application Generation

The Robot Operating System (ROS) is a framework to build robot applications.

Ogma is able to generate ROS monitoring applications that subscribe to obtain
the data needed by the monitors and report any violations. At present, support
for ROS app generation is considered preliminary.

ROS applications are generated using the Ogma command `ros`, which receives
five main arguments:
- `--app-target-dir DIR`: location where the ROS application files must be
  stored.
- `--fret-file-name FILENAME`: a file containing a FRET component specification.
- `--variable-file FILENAME`: a file containing a list of variables that must
be made available to the monitor.
- `--variable-db FILENAME`: a file containing a database of known variables,
and the topic they are included with.
- `--handlers FILENAME`: a file containing a list of handlers used in the
  specification.

Not all arguments are mandatory. You should always provide either a FRET
component specification, or both a variable file and a handlers file. If you
provide a variables file or a handler file _and_ a FRET component
specification, the variables/handlers file will always take precedence, and the
variables/requirements listed in the FRET component specification file will be
ignored.

The following execution generates an initial ROS application for runtime
monitoring using Copilot:
```sh
$ ogma ros --fret-file-name Export.json --variable-file variables --variable-db ros-variable-db --app-target-dir ros_demo
```

The application generated by Ogma contains the following files:
```
ros_demo/CMakeLists.txt
ros_demo/src/copilot_monitor.cpp
ros_demo/src/copilot_logger.cpp
ros_demo/src/.keep
ros_demo/package.xml
```

### Current limitations

The user must place the code generated by Copilot monitors in two files,
`ros_demo/src/monitor.h` and `ros_demo/src/monitor.c`. No Copilot or C code for
the monitors is generated by default.

The code generated by default assumes that handlers receive no arguments. The
user must modify the handlers accordingly if that is not the case.

Although the variable DB file is not mandatory, it is in practice required to
monitor any requirement that uses any input data: no topic subscriptions will
be generated for any variables for which a DB entry cannot be found. At present,
Ogma will proceed without warnings if a variable is mentioned in a requirement
or variables file but a matching entry is not found in the variable DB.

## F' Component Generation

F' (FPrime) is a component-based framework for spaceflight applications.

Ogma is able to generate F' monitoring components that subscribe to obtain
the data needed by the monitors and report any violations. At present, support
for F' component generation is considered preliminary.

F' components are generated using the Ogma command `fprime`, which receives
five main arguments:
- `--app-target-dir DIR`: location where the F' application files must be
  stored.
- `--fret-file-name FILENAME`: a file containing a FRET component specification.
- `--variable-file FILENAME`: a file containing a list of variables that must
be made available to the monitor.
- `--variable-db FILENAME`: a file containing a database of known variables,
and their types.
- `--handlers FILENAME`: a file containing a list of handlers used in the
  specification.

Not all arguments are mandatory. You should always provide either a FRET
component specification, or both a variable file and a handlers file. If you
provide a variables file or a handler file _and_ a FRET component
specification, the variables/handlers file will always take precedence, and the
variables/requirements listed in the FRET component specification file will be
ignored.

The following execution generates an initial F' component for runtime
monitoring using Copilot:
```sh
$ ogma fprime --fret-file-name Export.json --variable-db fprime-variable-db --app-target-dir fprime_demo
```

The component generated by Ogma contains the following files:
```
fprime_demo/CMakeLists.txt
fprime_demo/Copilot.fpp
fprime_demo/Copilot.cpp
fprime_demo/Copilot.hpp
fprime_demo/Dockerfile
fprime_demo/inline-copilot
```

For completion, the following execution should compile the produced monitoring
component in a docker environment (assuming that the necessary `Export.json`,
`fprime-variable-db` files exist, they have consistent information, etc.) using
FPrime's Reference Application:

```sh
$ ogma fprime --fret-file-name Export.json --variable-db fprime-variable-db --app-target-dir fprime_demo
$ ogma fret-component-spec --fret-file-name Export.json > Spec.hs
$ sed -i -e 's/compile "fret"/compile "copilot"/g' Spec.hs
$ cd fprime_demo/
$ runhaskell ../Spec.hs
$ docker build -t fprime .
```

### File formats

The format of the variables, variable DB, and handlers file are as follows.

The variables file can contain a list of variables used in a specification, one
per line. For example, if we are working with a specification that uses three
boolean variables called `autopilot`, `sensorLimitsExceeded`, and `pullup`, we
can provide them to Ogma's `fprime` command in a file like the following:
```sh
$ cat variables
autopilot
sensorLimitsExceeded
pullup
```

The variables database file contains a list of known variables and their types.
It does not matter if there are variables that are not used for one particular
specification, FRET file, or requirement/monitor. The only thing that matters
is that the variables used, and their types, be listed in the file. Continuing
with the same example, we could have:

```sh
$ cat fprime-variable-db
("temperature", "uint8_t")
("autopilot", "bool")
("sensorLimitsExceeded", "bool")
("pullup", "bool")
("current_consumption", "float")
```

In our example, we only care about the boolean variables; it is sufficient that
they be listed in the variable DB file.

Finally, the handlers file is a list of monitor handlers that the generated
FPrime component should restrict to monitoring. They are listed one per line:
```sh
$ cat handlers
handlerpropREQ_001
```

Note that the handler name must match the one used by Copilot. Ogma transforms
requirement names to ensure that they corresponding handlers are valid C
identifiers. For example, the Ogma-generated monitor for a FRET requirement
`REQ_001` would, upon violation, call a C handler `handlerpropREQ_001`. The
transformation only applies if you are working with FRET files and not directly
with other source languages.

### Current limitations

The user must place the code generated by Copilot monitors in three files,
`fprime_demo/src/copilot.h`, `fprime_demo/src/copilot_types.h` and
`fprime_demo/src/copilot.c`. No Copilot or C code for the monitors is generated
by default by the `fprime` command.

The code generated by default assumes that handlers receive no arguments. The
user must modify the handlers accordingly if that is not the case.

## Struct Interface Generation

A lot of the information that must be monitored in real-world C applications is
packed in structs. Copilot allows accessing specific fields of C structs, but
requires additional definitions in the Copilot language to make the shape of
those structs known to the compiler.

Ogma is able to generate the boilerplate code needed to work with C structs in
Copilot. For example, to use the following struct as the type of an extern
stream in Copilot, the user is expected to define several Copilot (Haskell)
types and type class instances:

```c
typedef struct {
   double x;
   double y;
} point;
```

Ogma can generate that code automatically with the `structs` subcommand:

```haskell
$ ogma structs --header-file-name examples/point.h
data Point = Point
  { pX :: Field "x" Double
  , pY :: Field "y" Double
  }

instance Struct Point where
  typename _ = "point"
  toValues v = [ Value Double (pX v), Value Double (pY v) ]

instance Typed Point where
  typeOf = Struct (Point (Field 0) (Field 0))

```

By including these definitions in a Copilot file, users can now access the
individual `x` and `y` fields of a `Point` in a stream.

# Contributions
<sup>[(Back to top)](#table-of-contents)</sup>

The best way to contribute to Ogma is to report any issues you find via the
issue tracker, and to use Ogma to build applications or in your own research
and let us know about your results.

We kindly ask users not to send PRs to this project. Instead, please document
the bugs you find or other suggestions as issues and we will make the necessary
changes.

# Acknowledgements
<sup>[(Back to top)](#table-of-contents)</sup>

Ogma has been created by Ivan Perez and Alwyn Goodloe.

The Ogma team would like to thank Dimitra Giannakopoulou, Anastasia Mavridou,
and Thomas Pressburger, from the FRET Team at NASA Ames, for the continued
input during the development of Ogma.

We would also like to thank Cesar Munoz and Swee Balachandran, for their help
with the cFS backend.

X-Plane images obtained via the X-Plane 10 (Pro) flight simulator. Re-shared
with permission.

# License
<sup>[(Back to top)](#table-of-contents)</sup>

Copyright 2020-2021 United States Government as represented by the
Administrator of the National Aeronautics and Space Administration. All Rights
Reserved.

See the file LICENSE.pdf for details.
