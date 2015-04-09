# Gidl

Gidl (for Galois Interface Description Language) is a simple IDL for
describing structured types.

## IDL format

See example in tests/example.idl. Currently, the format is not set in stone -
revisions coming shortly.

## Backends

Gidl currently has backends for:
  - Native Haskell
  - [Ivory][] language
  - [Tower][] wrapper over Ivory

## Build and Test
Use the `create-sandbox` target in the Makefile to create a local cabal
sandbox and install all dependencies.

The default target builds the gidl library. You can then use `cabal run gidl --
<OPTIONS>` to run the code generator. Use the `--help` option to get usage
information.

Use the `test` target in the Makefile to generate and test each backend
implementation.
