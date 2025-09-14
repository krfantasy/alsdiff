# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build System

This is an OCaml project using Dune build system:
- `dune build` - Build the project
- `dune runtest` - Run all tests
- `dune exec live_git` - Run the main executable
- `dune test test_upath` - Run specific test (Upath tests)
- `dune test test_live_git` - Run specific test (Live Git tests)

## Project Structure

- `bin/main.ml` - Main executable entry point
- `lib/` - Core library modules:
  - `xml.ml` - XML parsing and data structures
  - `upath.ml` - XPath-like query language for XML
  - `live.ml` - Ableton Live specific data structures and logic
  - `file.ml` - File handling and .als file decompression
  - `diff.ml` - Diffing functionality for Live objects
  - `patch.ml` - Patch data structures
- `test/` - Test suites:
  - `test_upath.ml` - Tests for XPath-like functionality
  - `test_live_git.ml` - Tests for Live Git functionality

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
- Indexing (`[0]`, `[1]`)
- Wildcards (`*`, `**`)
- Path navigation (`/tag1/tag2`)

## Development Commands

- `dune build @fmt` - Format code
- `dune promote` - Promote generated files
- `dune clean` - Clean build artifacts