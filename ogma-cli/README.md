# OGMA

Ogma is a tool to facilitate the integration of safe runtime monitors into
other systems. Ogma extends
[Copilot](https://github.com/Copilot-Language/copilot), a high-level runtime
verification framework that generates hard real-time C99 code.

# Features

- Generating the glue code necessary to work with C structs in Copilot.

## Table of Contents

- [Installation](#installation)
  - [Pre-requisites](#pre-requisites)
  - [Compilation](#compilation)
- [Usage](#usage)
  - [Struct Interface Generation](#struct-interface-generation)
- [Contributions](#contributions)
- [Acknowledgements](#acknowledgements)
- [License](#license)


# Installation
<sup>[(Back to top)](#table-of-contents)</sup>

## Pre-requisites
<sup>[(Back to top)](#table-of-contents)</sup>

To install Ogma from source, users must have the tools GHC and Cabal installed.
At this time, we recommend GHC 8.6 and a version of Cabal between 2.4 and 3.2.

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

Once GHC and cabal are installed, the simplest way to install ogma is with:
```sh
$ git clone https://github.com/nasa/ogma.git
$ cd ogma
$ cabal v1-sandbox init
$ cabal v1-sandbox add-source ogma-*/
$ cabal v1-install alex happy
$ cabal v1-install ogma-cli/ copilot
```

After that, the ogma executable will be generated in the directory
`./.cabal-sandbox/bin/`, which one can add to the `PATH` with:
```
$ export PATH="$PWD/.cabal-sandbox/bin/:$PATH"
```

# Usage
<sup>[(Back to top)](#table-of-contents)</sup>

The main invocation of ogma with `--help` lists sub-commands available:
```sh
$ ogma --help
ogma - an anything-to-Copilot application generator

Usage: ogma COMMAND
  Generate complete or partial Copilot applications from multiple languages

Available options:
  -h,--help                Show this help text

Available commands:
  structs                  Generate Copilot structs from C structs
```

## Struct Interface Generation

A lot of the information that must be monitored in real-world C applications is
packed in structs. Copilot allows accessing specific fields of C structs, but
requires additional definitions in the Copilot language to make the structure
of those structs known to the compiler.

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

# License
<sup>[(Back to top)](#table-of-contents)</sup>

Copyright 2020 United States Government as represented by the Administrator
of the National Aeronautics and Space Administration. All Rights Reserved.

See the file LICENSE.pdf for details.
