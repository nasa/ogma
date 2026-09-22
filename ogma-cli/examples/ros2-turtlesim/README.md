# Generating monitors for ROS 2 Turtlesim demo

This directory contains a project that can be used to generate a runtime
monitor for the [ROS 2 Turtlesim
simulation](https://docs.ros.org/en/humble/Tutorials/Beginner-CLI-Tools/Introducing-Turtlesim/Introducing-Turtlesim.html).

## Table of Contents

- [Introduction](#introduction)
- [Scenario](#scenario)
- [Code generation](#code-generation)
- [Compilation](#compilation)
- [Execution](#execution)

# Introduction
<sup>[(Back to top)](#table-of-contents)</sup>

Turtlesim is a tutorial designed to help understand ROS 2. It presents a
simple, 2D simulation of a turtle, which can be controlled using the keyboard.

# Scenario
<sup>[(Back to top)](#table-of-contents)</sup>

For the sake of the example, we seek to ensure that the turtle remains close to
the central column of the screen. Given that the position of the turtle at the
beginning of the simulation is close to X=5.5, we decide to require that it be
greater than 1 and lower than 10.

To that end, we shall use Ogma to generate a ROS 2 node that continuously
monitors the position of the turtle. Whenever it goes out of the bounds
indicated, the monitoring node shall publish a message that alerts of the
violation.

The file `ogma-cli/examples/ros2-turtlesim/document-turtlesim.json` contains
the property:

```json
"properties": [
  { "id":      "KeepTurtleInCheck",
    "formula": "input_signal >= 1 && input_signal < 10",
    "text":    "The turtle stays near the central column of the screen"
  }
]
```

where `input_signal` is a float representing the horizontal coordinate of the
turtle's position. The `formula` represents the formal encoding of the
property, and the `text` represents a less precise description of the property
in natural language.

# Code generation
<sup>[(Back to top)](#table-of-contents)</sup>

To generate monitors for the Turtlesim demo using the project in this
directory, we run, from the top level directory of a clone of the `ogma`
repository:

```sh
ogma ros --project ogma-cli/examples/ros2-turtlesim/project.ogma
```

The project already contains auxiliary information about messages being
published by Turtlesim, types, dependencies, and the connection to variables
mentioned in the property listed above.

The call to `ogma` generates a `turtlesim-demo` directory at the top level of
the repo with a Copilot spec in a file `copilot/src/Copilot.hs`. The spec
constitutes a formally verifiable executable implementation of the properties
specified in `ogma-cli/examples/ros2-turtlesim/document-turtlesim.json`.

To compile that specification, we use `runhaskell` from the directory where it
is located, which should generate three C files containing the core of the
monitoring code:

```sh
cd turtlesim-demo/copilot/src/
runhaskell Copilot.hs
cd ../../../
```

Finally, we copy the local files `manual-deps.repos` and
`manually-installed-pkgs.txt` into the target directory:

```sh
cp ogma-cli/examples/ros2-turtlesim/manual-deps.repos turtlesim-demo/
cp ogma-cli/examples/ros2-turtlesim/manually-installed-pkgs.txt turtlesim-demo/
```

# Compilation
<sup>[(Back to top)](#table-of-contents)</sup>

Now, we build the complete example using `docker`:


```sh
cd turtlesim-demo/
docker build -t ogma-turtlesim-demo .
```

The build process will take some time. After it completes, it should create a
docker image called `ogma-turtlesim-demo` containing all the code needed for
the demonstration.

# Execution
<sup>[(Back to top)](#table-of-contents)</sup>

Once the image builds, we can test the resulting monitor in the context of the
Turtlesim simulation by executing several commands on 4 different terminals.

## Terminal 1

```
$ xhost +
$ docker run --rm -it \
   --name ogma-turtlesim-demo-container \
   --network host \
   -e DISPLAY \
   -e TERM \
   -e QT_X11_NO_MITSHM=1 \
   -v /tmp/.X11-unix:/tmp/.X11-unix:rw \
   ogma-turtlesim-demo \
   /bin/bash
```

Once container boots, we start the simulation itself, which brings up ta GUI
with the turtle in the middle:

```
$ source install/setup.bash
$ ros2 run turtlesim turtlesim_node
```

That should bring up a GUI with a turtle in the middle.

## Terminal 2

In the second terminal we execute:

```
$ docker exec -it ogma-turtlesim-demo-container /bin/bash
```

Once the container boots, we run a ROS 2 that allows us to control the turtle
using the keyboard:

```
$ source install/setup.bash
$ ros2 run turtlesim turtle_teleop_key
```

## Terminal 3

In the third terminal we execute:

```
$ docker exec -it ogma-turtlesim-demo-container /bin/bash
```

Once the container boots, we start the monitoring node, which awaits for
message from the turtle to be received:

```
$ source install/setup.bash
$ ros2 run copilot copilot
```

## Terminal 4

```
$ docker exec -it ogma-turtlesim-demo-container /bin/bash
```

Once the container boots, we listen for message violations reported by the
monitoring node via a dedicated topic:

```
$ source install/setup.bash
$ ros2 topic echo /copilot/handlerKeepTurtleInCheck \
    | while IFS= read -r line; do \
        echo "$(date '+%Y-%m-%d %H:%M:%S') $line"; \
      done
```

We can now move the turtle around using the arrow keys in Terminal 2. Whenever
the turtle is moved close to the left or right edges of the screen, Terminal 4
will print messages from the Copilot monitoring node detecting the violation.
