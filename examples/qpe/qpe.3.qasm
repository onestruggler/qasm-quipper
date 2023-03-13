OPENQASM 3;

include "stdgates.inc";

qubit[3] x;
qubit phi;

reset x[0];
reset x[1];
reset x[2];

h x[0];
h x[1];
h x[2];

pow(pow(2, 2)) @ ctrl @ t x[0], phi; 
pow(pow(2, 1)) @ ctrl @ t x[1], phi;
pow(pow(2, 0)) @ ctrl @ t x[2], phi;

h x[0];
crz(pi / 2) x[1], x[0];
crz(pi / 4) x[2], x[0];
h x[1];
crz(pi / 2) x[2], x[1];
h x[2];

measure x[0];
measure x[1];
measure x[2];
