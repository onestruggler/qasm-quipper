// Implements QFT on 4 qubits using version 2.0 of OpenQASM.

OPENQASM 2.0;

include "qelib1.inc";

qreg q1;
qreg q2;
qreg q3;
qreg q4;

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