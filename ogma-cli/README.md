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

## Table of Contents

- [Installation](#installation)
  - [Pre-requisites](#pre-requisites)
  - [Compilation](#compilation)
- [Usage](#usage)
  - [Language Transformations: FRET](#language-transformations-fret)
  - [cFS Application Generation](#cfs-application-generation)
  - [Struct Interface Generation](#struct-interface-generation)
- [Contributions](#contributions)
- [Acknowledgements](#acknowledgements)
- [License](#license)


# Installation
<sup>[(Back to top)](#table-of-contents)</sup>

## Pre-requisites
<sup>[(Back to top)](#table-of-contents)</sup>

To install Ogma from source, users must have the tools GHC and cabal-install.
At this time, we recommend GHC 8.6 and a version of cabal-install between 2.4
and 3.2. (Ogma has been tested with GHC versions up to 8.10 and cabal-install
versions up to 3.6, although the installation steps may vary slightly depending
on the version of cabal-install being used.)

On Debian or Ubuntu Linux, both can be installed with:

```sh
$ apt-get install ghc cabal
```

On Mac, they can be installed with:

```sh
$ brew install ghc cabal
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
  fret-component-spec      Generate a Copilot file from a FRET Component
                           Specification
  fret-reqs-db             Generate a Copilot file from a FRET Requirements
                           Database
```

## Language transformations: FRET
<sup>[(Back to top)](#table-of-contents)</sup>

Ogma can convert specifications written in other languages to Copilot monitors,
such as the ones supported by [NASA's requirements elicitation tool
FRET](https://github.com/NASA-SW-VnV/fret). The commands `fret-component-spec`
and `fret-reqs-db` allow users to interact with the different kinds of files
produced by FRET.

FRET files include properties encoded using Temporal Logic, both in
[SMV](http://www.cs.cmu.edu/~modelcheck/smv.html) and in
[CoCoSpec](https://link.springer.com/chapter/10.1007%2F978-3-319-41591-8_24),
the latter of which is based on Lustre. Ogma uses the SMV expressions by
default, but the CLI flag `--cocospec` can be used to select the CoCoSpec
variant of requirements instead.

For example, from the following FRET requirement:
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
includes a hard real-time C99 implementation of the monitor. The FRET
specification example above is included with the Ogma distribution, and can be
tested with:

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

Ogma includes multiple facilities to generate NASA's core Flight System
(cFS) applications. The cFS applications generated by Ogma perform three
simple steps to connect Copilot monitors to cFS:
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

# License
<sup>[(Back to top)](#table-of-contents)</sup>

Copyright 2020-2021 United States Government as represented by the
Administrator of the National Aeronautics and Space Administration. All Rights
Reserved.

See the file LICENSE.pdf for details.
