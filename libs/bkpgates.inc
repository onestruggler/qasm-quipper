gate swap a, b {
    cx a, b;
    cx b, a;
    cx a, b;  
}

gate cswap c, a, b {
    cx a, b;
    ccx c, b, a;
    cx a, b;
}

gate sx a {
    // This gate is off by a global phase of -1. However, controls are not a
    // feature in OpenQASM 2.0, so the global phase is unobservable.
    h a;
    sdg a;
    h a;
}

gate p(theta) a {
    // This gate is off by a global phase of theta / 2. However, controls are
    // not a feature in OpenQASM 2.0, so the global phase is unobservable.
    u1(theta) a;
}

gate cp(theta) c, t {
    p(theta / 2) t;
    cx c, t;
    p(-theta / 2) t;
    cx c, t;
    p(theta / 2) c;
}

gate crx(theta) c, t {
    rx(theta / 2) t;
    cz c, t;
    rx(-theta / 2) t;
    cz c, t;
}

gate cry(theta) c, t {
    ry(theta / 2) t;
    cz c, t;
    ry(-theta / 2) t;
    cz c, t;
}

gate cu(theta, phi, lambda, gamma) c, t {
    p(gamma) c;
    p((phi+lambda)/2) c; // U-gates in OpenQASM 2.0 and 3 differ by a phase.
    cu3(theta, phi, lambda) c, t;
}
