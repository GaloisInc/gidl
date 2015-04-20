# Gidl

Gidl (for Galois Interface Description Language) is a simple IDL for
describing structured types and RPC-style interfaces.

Gidl has a type language which permits the user to define types using
the following primitives:
- Atomic types:
    - Signed integers of 8, 16, 32, 64 bit width
    - Unsigned integers of 8, 16, 32, 64 bit width
    - IEEE 754 single and double precision floating point numbers
    - Boolean values
- User-defined Enum types:
    - Pairs of names and values, where names and values must have
      a one-to-one correspondence
    - User specified representation width (8, 16, 32, or 64 bits)
- User-defined Newtypes:
    - Wraps an existing atomic or enum type with a new type
- User-defined Structures:
    - Set of named fields. Corresponds to a record or a C struct.
    - All fields are atomic, enum, or newtypes.

Gidl interfaces are composed of the following primitives:
- Streams, which are sent from server to client periodically
- Attributes, which are read and written according to requests by the client.
  Attributes have a user defined read/writable permissions.

Interfaces can be composed by subtyping - java style multiple inheritance.
(We expect interface composition may change in the future.)

Protocol drift is detected by identifying each stream and attribute message
on the wire by a hash of its name, its type, and all child types. Therefore

## IDL format

The gidl IDL uses a s-expression based format.

See example format: `tests/example.idl`

## Backends

Gidl currently has backends for:
  - Native Haskell
  - [Ivory][] types, and [Tower][] interfaces

[Ivory]: http://github.com/GaloisInc/ivory
[Tower]: http://github.com/GaloisInc/tower

## Build and Test
Use the `create-sandbox` target in the Makefile to create a local cabal
sandbox and install all dependencies.

The default target builds the gidl library. You can then use `cabal run gidl --
<OPTIONS>` to run the code generator. Use the `--help` option to get usage
information.

Use the `test` target in the Makefile to generate and test each backend
implementation.
