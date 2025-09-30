# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build System

This is an OCaml project using Dune build system:
- `dune build` - Build the project
- `dune runtest --force` - Run all tests
- `dune exec live_git` - Run the main executable
- `dune test test_upath` - Run specific test (Upath tests)

## Project Structure

- `bin/main.ml` - Main executable entry point
- `lib/` - Core library modules:
  - `xml.ml` - XML parsing and data structures
  - `upath.ml` - XPath-like query language for XML
  - `live.ml` - Ableton Live specific data structures and logic
  - `file.ml` - File handling and .als file decompression
- `test/` - Test suites:
  - `test_upath.ml` - Tests for XPath-like functionality

## Key Dependencies

- `xmlm` - XML parsing
- `camlzip` - Gzip decompression for .als files
- `angstrom` - Parser combinators for Upath
- `eio` - Effects-based IO
- `alcotest` - Testing framework

## Architecture Overview

This is a Git helper tool for Ableton Live Set (.als) files. The core functionality:

1. **File Handling**: Decompress .als files (which are gzipped XML)
2. **XML Processing**: Parse and navigate XML structure of Live sets
3. **Upath**: Custom XPath-like query language for finding elements in XML
4. **Diffing**: Compare Live set objects to detect changes
5. **Patch Management**: Handle patches and modifications

The `Upath` module provides a subset of XPath functionality with support for:
- Tag names with attributes (`tag@attr="value"`)
- Indexing with optional tag matching (`[0]`, `[1]`, `tag[0]`, `tag[1]`)
- Wildcards (`*`, `**`)
- Path navigation (`/tag1/tag2`)

## Development Commands

- `dune build @fmt` - Format code
- `dune promote` - Promote generated files
- `dune clean` - Clean build artifacts
- `dune utop` - Load this library into Utop REPL
- `dune utop . -- -emacs` - Load this library into Utop REPL for Emacs utop-mode
