# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build System

This is an OCaml project using Dune build system:
- `dune build` - Build the project
- `dune runtest` - Run all tests
- `dune runtest --force` - Force rerun all tests
- `dune exec alsdiff` - Run the main executable
- `dune test test_upath` - Run specific test (Upath tests)
- `dune test test_diff_list_myers` - Run Myers diffing algorithm tests
- `dune test test_diff_automation` - Run automation diffing tests

## Project Structure

- `bin/main.ml` - Main executable entry point
- `lib/` - Core library modules:
  - `xml.ml` - XML parsing and data structures
  - `upath.ml` - XPath-like query language for XML
  - `live.ml` - Ableton Live specific data structures and logic
  - `file.ml` - File handling and .als file decompression
  - `diff.ml` - Diffing algorithms including Myers O(ND) implementation
  - `output.ml` - Output formatting and display
  - `config.ml` - Configuration management
  - `time.ml` - Time-related utilities
  - `equality.ml` - Equality checking utilities
- `test/` - Test suites:
  - `test_upath.ml` - Tests for XPath-like functionality
  - `test_xml.ml` - Tests for XML parsing
  - `test_live.ml` - Tests for Live set functionality
  - `test_wildcard.ml` - Tests for wildcard matching
  - `test_complex.ml` - Complex integration tests
  - `test_diff_automation.ml` - Tests for diffing automation envelopes
  - `test_diff_list_ord.ml` - Tests for ordered list diffing
  - `test_diff_list_myers.ml` - Tests for Myers diffing algorithm

## Key Dependencies

- `xmlm` - XML parsing
- `camlzip` - Gzip decompression for .als files
- `angstrom` - Parser combinators for Upath
- `eio` - Effects-based IO
- `alcotest` - Testing framework
- `yojson` - JSON parsing and serialization
- `ppx_deriving.eq` - PPX extension for deriving equality functions
- `ppx_deriving_jsonschema` - PPX extension for JSON schema generation
- `ppx_deriving_yojson` - PPX extension for Yojson serialization

## Architecture Overview

This is a Git helper tool for Ableton Live Set (.als) files. The core functionality:

1. **File Handling**: Decompress .als files (which are gzipped XML)
2. **XML Processing**: Parse and navigate XML structure of Live sets
3. **Upath**: Custom XPath-like query language for finding elements in XML
4. **Diffing**: Compare Live set objects to detect changes with advanced algorithms
5. **Patch Management**: Handle patches and modifications
6. **Output Formatting**: Format and display results

The `Upath` module provides a subset of XPath functionality with support for:
- Tag names with attributes (`tag@attr="value"`)
- Indexing with optional tag matching (`[0]`, `[1]`, `tag[0]`, `tag[1]`)
- Wildcards (`*`, `**`)
- Path navigation (`/tag1/tag2`)

The `Diff` module implements multiple diffing algorithms:
- Ordered diffing for sequential data
- Myers O(ND) algorithm for optimal diffing performance
- Specialized diffing for automation envelopes in Live sets

## Development Commands

- `dune build @fmt` - Format code
- `dune promote` - Promote generated files
- `dune clean` - Clean build artifacts
- `dune utop` - Load this library into Utop REPL
- `dune utop . -- -emacs` - Load this library into Utop REPL for Emacs utop-mode

## Git commit message convention
Always adding the texts inside the code block in the end of git commit message,
```

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>
```
