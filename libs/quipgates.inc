gate quip_rz(theta) a {
    // Legacy support for OpenQASM 2.0:
    U(0, theta, theta) a; // Equivalent to gphase(theta).
    rz(2 * theta) a;
}

gate quip_ix a {
    y a;
    s a;
    y a;
    s a;
    x a;
}

gate quip_omega a {
    // Legacy support for OpenQASM 2.0:
    U(0, pi / 4, pi / 4) a; // Equivalent to gphase(pi / 4).
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
