gate quip_ix a {
    y a;
    s a;
    y a;
    s a;
    x a;
}

gate quip_omega a {
    // The omega gate is a specific global phase gate. Since controls are not
    // possible in OpenQASM 2.0, then this gate is a no-op in OpenQASM 2.0.
    // Therefore, this gate can implement any global phase in OpenQASM 2.0, so
    // long as it implements a global phase of exp(i*pi/4) in OpenQASM 3.
    p(pi / 4) a;
    x a;
    p(pi / 4) a;
    x a;
}

gate quip_e a {
    quip_omega a;
    quip_omega a;
    quip_omega a;
    s a;
    s a;
    s a;
    h a;
}

gate quip_w a, b {
    cx b, a;
    cx a, b;
    ch a, b;
    cx a, b;
    cx b, a;
}
