gate swap a, b {
    cx a, b;
    cx b, a;
    cx a, b;  
}

gate cswap a, b, c {
    ccx a, b, c;
    ccx a, c, b;
    ccx a, b, c;
}

gate sx a {
    U(0, pi, pi) a;
    h a;
    sdg a;
    h a;
    x a;
}

gate p(theta) a {
    // Legacy support for OpenQASM 2.0:
    U(0, theta / 2, theta / 2) a; // Equivalent to gphase(theta / 2).
    rz(theta) a;
}

gate cp(theta) a, b {
    p(theta / 2) b;
    cx a, b;
    p(-theta / 2) b;
    cx a, b;
}

gate crx(theta) a, b {
    rx(theta / 2) b;
    cx a, b;
    rx(-theta / 2) b;
    cx a, b;
}

gate cry(theta) a, b {
    ry(theta / 2) b;
    cx a, b;
    ry(-theta / 2) b;
    cx a, b;
}
