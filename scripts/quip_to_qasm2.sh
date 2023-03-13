#!/bin/bash

source $(dirname "$0")/lingua_quanta_utils.sh

# Parses command-line argument flags, and populates default values. The
# command-line arguments are stored to global variables. The following
# arguments are supported.
#   -s      the source file (Quipper)                           stored to $src
#   -o      the output file (OpenQASM 2.0)                      stored to $dst
#   -t      a directory to store all intermediate results.      stored to $tmp
# Reads user arguments.
while getopts 's:o:t:' OPTION
do
    case "$OPTION" in
        s) src=${OPTARG};;
        o) dst=${OPTARG};;
        t) tmp=${OPTARG};;
    esac
done
if [ -z "${src}" ]; then error_exit "Expected source file (-s)!"; fi
if [ -z "${dst}" ]; then error_exit "Expected output file (-o)!"; fi
if [ -z "${tmp}" ]; then tmp=$(mktemp -d); fi

# Names intermediate files.
cmd1fn="${tmp}/tmp.elim_ctrls.quip"
cmd2fn="${tmp}/tmp.quip_to_qasm.qasm"
cmd3fn="${tmp}/tmp.elim_pows.qasm"
cmd4fn="${tmp}/tmp.elim_invs.qasm"
cmd5fn="${tmp}/tmp.elim_funs.qasm"
cmd6fn="${tmp}/tmp.formate_qasm.qasm"

# Phase 1: Control elimination.
#
# Eliminates controls not found in the OpenQASM 3 standard library.

elim_ctrls --src=${src} --out=${cmd1fn}
if [ $? -ne 0 ]; then error_exit "Failure in elim_ctrls."; fi

# Phase 2: Translation
#
# A direct translation from Quipper without controls to OpenQASM without
# control modifiers.

quip_to_qasm --src=${cmd1fn} --out=${cmd2fn}
if [ $? -ne 0 ]; then error_exit "Failure in quip_to_qasm."; fi

# Phase 3: Modifier elimination.
#
# Eliminates all remaining OpenQASM 3 modifiers. Note that control modifiers
# were already eliminated during during Phase 1, and that Phase 2 does not
# introduce new control modifiers by design.

elim_pows --src=${cmd2fn} --out=${cmd3fn}
if [ $? -ne 0 ]; then error_exit "Failure in elim_pows."; fi

elim_invs --src=${cmd3fn} --out=${cmd4fn}
if [ $? -ne 0 ]; then error_exit "Failure in elim_invs."; fi

elim_funs --src=${cmd4fn} --out=${cmd5fn}
if [ $? -ne 0 ]; then error_exit "Failure in elim_funs."; fi

# Phase 4: Formatting.
#
# Converts the OpenQASM 3 syntax to OpenQASM 2.0 syntax. If the translation
# reaches this stage, then the remaining statements belong to the OpenQASM 2.0
# subset of OpenQASM 3.

format_qasm --legacy --src=${cmd5fn} --out=${dst}
if [ $? -ne 0 ]; then error_exit "Failure in format_qasm."; fi
