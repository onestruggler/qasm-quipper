# LinguaQuanta Executables

This directory is used to build tools for program translation and
transformation. Additional, this directory also contains tools for
program formatting and toolchain debugging.

## Program Translation Tools

Program translation tools include the following.
- **quip_to_qasm**: A tool to translate Quipper ASCII circuits into OpenQASM
  3 programs.
- **qasm_to_quip**: A tool to translate both OpenQASM 2.0 and OpenQASM 3
  programs to Quipper ASCII circuits.
- **to_lsc**: A tool to translate from the LSC subset of OpenQASM 2.0 to the
  Lattice Surgery Compiler input language. Note that multiple transformations
  are required to reach the LSC subset of OpenQASM 2.0 given an arbitrary
  OpenQASM 3 program (see the `quip_to_lsc.sh` script for more details).

## Program Transformation Tools

Program transformation tools include the following.
- **elim_ctrls**: A tool to reduce each controlled gate in a Quipper program to
  a sequence of uncontrolled and singly controlled unitaries. Command-line
  arguments are provided to decide how special gates, such as the Tofolli and
  Fredkin gates, are handled.
- **elim_pows**: A tool to eliminate all pow modifiers in an OpenQASM program.
- **elim_invs**: A tool to eliminate all inv modifiers in an OpenQASM program.
- **elim_funs**: A tool to eliminate all function calls in an OpenQASM program.
  In particular, functions from `quipfuncs.inc` are inlined.

## Program Formatting Tools

Program formatting tools include the following.
- **reg_merge**: A tool to merge all quantum registers in an OpenQASM program.
- **format_qasm**: A tool to format OpenQASM 3 programs using the OpenQASM 2.0
  subset of the language.
- **format_quip**: A tool to convert from a Quipper ASCII circuit to other
  valid Quipper output formats (e.g., PDF files and gate counts).

## LinguaQuanta Debugging Tools

The tools `quip_tools` and `qasm_tools` are provided for toolchain debugging.
These tools allow a developer to inspect a Quipper or OpenQASM program after
each of: (1) parsing; (2) statement abstraction; (3) code generation. Note that
stage (2) refers to reading in the categorical model of translation and (3)
refers to writing in the categorical model of translation. This means that
code generation should be an idempotent operation (`w,r,w,r = w,ID,r = w,r`).
