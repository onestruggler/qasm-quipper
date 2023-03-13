# LinguaQuanta OpenQASM Libraries

The translation from Quipper to OpenQASM 3 relies on certain gates that do not
appear in the OpenQASM standard library. Furthermore, certain features used by
the `quip_to_qasm` translator are not supported in version 2.0 of OpenQASM. For
this reason, the LinguaQuanta toolchain also provides three OpenQASM libraries
that are used across all translations.

## Overview of Libraries

The librarieas provided by LinguaQuanta include the following.
- **bkpgates.inc**: The `stdgates.inc` library for OpenQASM 3 provides gates
  not found in `qelib1.inc` for OpenQASM 2.0, such as the `crx` gate. This gap
  is bridged by the `bkpgates.inc` library, which backports all new gates in
  `stdgates.inc` using only those gates provided by `qelib1.inc`. The
  translations in this library are only correct upto a global phase, since the
  `ctrl` modifier is unique to OpenQASM 3.
- **quipgates.inc**: This library provides implementations for all gates found
  in the Quipper standard library, which are not provided by `stdgates.inc`.
  This library is implemented in the OpenQASM 2.0 compliant subset of OpenQASM
  3, and assumes access to all gates from `stdgates.inc`.
- **quipfuncs.inc**: This library provides functional implementations for all
  supported non-unitary gates in Quippers. Since OpenQASM 2.0 does not support
  user-defined functions, then this library is exclusive to OpenQASM 3. Note
  that the `elim_funs` tool may be used to inline these functions.
