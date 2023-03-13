// Implements QFT on 4 qubits using version 3 of OpenQASM.

OPENQASM 3;

include "stdgates.inc";

qubit q1;
qubit q2;
qubit q3;
qubit q4;

h q1;
crz(pi / 2) q2, q1;
crz(pi / 4) q3, q1;
crz(pi / 8) q4, q1;

h q2;
crz(pi / 2) q3, q2;
crz(pi / 4) q4, q2;

h q3;
crz(pi / 2) q4, q3;

h q4;