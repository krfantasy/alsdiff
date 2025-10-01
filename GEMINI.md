# Project Overview

This project, `live_git`, is a command-line tool written in OCaml for working with Ableton Live Set (`.als`) files. It provides utilities to parse, analyze, and manipulate the structure of these files, which are essentially compressed XML. The primary goal of this project is to provide a way to use Git for version control of Ableton Live projects, by making the `.als` files more diff-friendly.

## Key Technologies

*   **OCaml:** The core language for the project.
*   **Dune:** The build system used for OCaml projects.
*   **xmlm:** A library for parsing and manipulating XML in OCaml.
*   **camlzip:** A library for working with compressed files, used to decompress the `.als` files.

## Architecture

The project is structured into a library (`lib`) and a binary (`bin`).

*   **`lib`:** Contains the core logic for parsing and manipulating Ableton Live Sets.
    *   `live.ml`: Defines the data structures that represent the components of a Live Set, such as tracks, clips, scenes, and devices.
    *   `xml.ml`: Provides the functionality to read and parse the XML content of `.als` files.
    *   `upath.ml`: A module for working with paths within the XML structure.
    *   `diff.ml`: (Currently empty) Intended to contain the logic for diffing two Live Sets.
    *   `patch.ml`: Defines the data structure for a patch, which represents a change in a Live Set.
*   **`bin`:** Contains the executable entry point.
    *   `main.ml`: The main executable, which currently contains test code for the `Upath` module.

# Building and Running

This project uses the `dune` build system.

## Building

To build the project, run the following command:

```bash
dune build
```

This will compile the source code and create an executable in the `_build` directory.

## Running

To run the main executable, use the following command:

```bash
dune exec live_git
```

This will execute the `main.ml` file.

## Testing

To run the tests, use the following command:

```bash
dune runtest
```

# Development Conventions

*   **Build System:** All builds and tests are managed through `dune`.
*   **Dependencies:** Project dependencies are listed in the `dune-project` file.
*   **Code Style:** The code follows standard OCaml conventions.
*   **Versioning:** The project appears to be in its early stages of development. The `diff.ml` file is empty, suggesting that the diffing functionality is not yet implemented.
