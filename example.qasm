qubit[2+3] vars;
qreg old[3-1];
pow(5) @ inv @ ctrl @ negctrl(2) @ inv @ x vars[0], vars[1], vars[2], vars[3];
inv @ crz(pi/2) vars[0], vars[1];
mygate vars[0];
CX vars[1], old[3];
