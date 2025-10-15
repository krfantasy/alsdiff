# Project Overview

This project, `alsdiff`, is a command-line tool written in OCaml for working with Ableton Live Set (`.als`) files. It provides utilities to parse, analyze, and manipulate the structure of these files, which are essentially compressed XML. The primary goal of this project is to provide a way to use Git for version control of Ableton Live projects, by making the `.als` files more diff-friendly.

## Key Technologies

*   **OCaml:** The core language for the project.
*   **Dune:** The build system used for OCaml projects.
*   **xmlm:** A library for parsing and manipulating XML in OCaml.
*   **camlzip:** A library for working with compressed files, used to decompress the `.als` files.
*   **angstrom:** Parser combinators for Upath.
*   **eio:** Effects-based IO.
*   **alcotest:** Testing framework.
*   **yojson:** JSON parsing and serialization.
*   **ppx_deriving.eq:** PPX extension for deriving equality functions.
*   **ppx_deriving_jsonschema:** PPX extension for JSON schema generation.
*   **ppx_deriving_yojson:** PPX extension for Yojson serialization.

## Architecture

The project is structured into a library (`lib`) and a binary (`bin`).

*   **`lib`:** Contains the core logic for parsing and manipulating Ableton Live Sets, organized into sublibraries:
    *   `lib/base/` - Base functionality modules: `xml.ml`, `upath.ml`, `file.ml`, `equality.ml`.
    *   `lib/live/` - Ableton Live specific modules: `automation.ml`, `clip.ml`, `track.ml`, `device.ml`.
    *   `lib/diff/` - Diffing algorithms: `diff.ml`, `clip_patch.ml`, `automation_patch.ml`, `track_patch.ml`.
    *   `lib/output/` - Output formatting: `output.ml`, `text_output.ml`.
*   **`bin`:** Contains the executable entry point.
    *   `main.ml`: The main executable.

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
dune exec alsdiff
```

To run specific tests, use commands like:

```bash
dune exec test/test_upath.exe
dune exec test/test_diff_list.exe
dune exec test/test_diff_automation.exe
```

## Testing

To run all tests, use the following command:

```bash
dune runtest
```

# Project Structure

- `bin/main.ml` - Main executable entry point
- `lib/` - Core library modules organized into sublibraries:
  - `lib/base/` - Base functionality modules:
    - `xml.ml` - XML parsing and data structures
    - `upath.ml` - XPath-like query language for XML
    - `file.ml` - File handling and .als file decompression
    - `equality.ml` - Equality checking utilities
  - `lib/live/` - Ableton Live specific modules:
    - `automation.ml` - Automation envelope handling
    - `clip.ml` - Clip and mixer functionality
    - `track.ml` - Track handling and management
    - `device.ml` - Device and plugin functionality
  - `lib/diff/` - Diffing algorithms:
    - `diff.ml` - Core diffing algorithms including Myers O(ND) implementation
    - `clip_patch.ml` - Clip patching and modification handling
    - `automation_patch.ml` - Automation patching and modification handling
    - `track_patch.ml` - Track patching and modification handling
  - `lib/output/` - Output formatting:
    - `output.ml` - Output interface definitions
    - `text_output.ml` - Plain text output rendering
- `test/` - Test suites (all use specific module opens for cleaner code):
  - `test_upath.ml` - Tests for XPath-like functionality
  - `test_xml.ml` - Tests for XML parsing
  - `test_live.ml` - Tests for Live set functionality
  - `test_wildcard.ml` - Tests for wildcard matching
  - `test_wildcard_debug.ml` - Debug tests for wildcard matching
  - `test_complex.ml` - Complex integration tests
  - `test_audio_clip.ml` - Tests for audio clip functionality
  - `test_midi_clip.ml` - Tests for MIDI clip functionality
  - `test_diff_automation.ml` - Tests for diffing automation envelopes
  - `test_diff_list.ml` - Tests for list diffing algorithms (Myers and LCS)
  - `test_diff_mixer.ml` - Tests for mixer diffing functionality
  - `test_diff_audio_clip.ml` - Tests for audio clip diffing
  - `test_diff_midi_clip.ml` - Tests for MIDI clip diffing
  - `test_clip_patch.ml` - Tests for clip patching functionality
  - `utils.ml` - Shared test utilities

## Architecture Overview

This is a Git helper tool for Ableton Live Set (.als) files. The core functionality:

1.  **File Handling**: Decompress .als files (which are gzipped XML)
2.  **XML Processing**: Parse and navigate XML structure of Live sets
3.  **Upath**: Custom XPath-like query language for finding elements in XML
4.  **Diffing**: Compare Live set objects to detect changes with advanced algorithms
5.  **Patch Management**: Handle patches and modifications
6.  **Output Formatting**: Format and display results

The `Upath` module provides a subset of XPath functionality with support for:

*   Tag names with attributes (`tag@attr="value"`)
*   Indexing with optional tag matching (`[0]`, `[1]`, `tag[0]`, `tag[1]`)
*   Wildcards (`*`, `**`)
*   Path navigation (`/tag1/tag2`)

The `Diff` module implements multiple diffing algorithms:

*   Ordered diffing for sequential data
*   Myers O(ND) algorithm for optimal diffing performance
*   Specialized diffing for automation envelopes in Live sets

## Library Organization

The project is organized into four main libraries:

1.  **alsdiff_lib_base** (`lib/base/`) - Core functionality
2.  **alsdiff_lib_live** (`lib/live/`) - Ableton Live specific types and logic
3.  **alsdiff_lib_diff** (`lib/diff/`) - Diffing algorithms
4.  **alsdiff_lib_output** (`lib/output/`) - Output formatting

### Module Access Patterns

When working with the libraries, use specific module opens for cleaner code:

```ocaml
(* Base modules *)
open Alsdiff_lib_base.Xml
open Alsdiff_lib_base.Upath

(* Live modules *)
open Alsdiff_lib_live.Automation
open Alsdiff_lib_live.Clip
open Alsdiff_lib_live.Track
open Alsdiff_lib_live.Device

(* Diff modules *)
open Alsdiff_lib_diff.Diff
open Alsdiff_lib_diff.Clip_patch
open Alsdiff_lib_diff.Automation_patch
open Alsdiff_lib_diff.Track_patch

(* Output modules *)
open Alsdiff_lib_output.Text_output.TextOutput
```

This allows you to write `Automation.t` instead of `Alsdiff_lib_live.Automation.Automation.t` and `Xml.read_file` instead of `Alsdiff_lib_base.Xml.read_file`.

### Legacy Modules

The following modules have been refactored into the new library structure:

*   `config.ml`, `time.ml`, `eff.ml`, `gadt.ml`, `fraction.ml`, `oop.ml` - Moved to lib/base/ or removed
*   `output.ml` - Split into interface (`output.ml`) and implementation (`text_output.ml`) in lib/output/

## Project Cleanup
*   `live.ml` - Removed, as it was split into `automation.ml` and `clip.ml` in lib/live/

## Development Commands

*   `dune build @fmt` - Format code
*   `dune promote` - Promote generated files
*   `dune clean` - Clean build artifacts
*   `dune utop` - Load this library into Utop REPL
*   `dune utop . -- -emacs` - Load this library into Utop REPL for Emacs utop-mode
