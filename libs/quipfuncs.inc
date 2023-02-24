//
// Classical Ancillas.
//

def CInit0 -> bit {
    return 0;
}

def CInit1 -> bit {
    return 1;
}

def CTerm0 -> bit {
    return 0;
}

def CTerm1 -> bit {
    return 0;
}

def CDiscard -> bit {
    return 0;
}

//
// Quantum Ancillas.
//

def QInit0(qubit q) {
    reset q;
}

def QInit1(qubit q) {
    reset q;
    x q;
}

def QTerm0(qubit q) {}

def QTerm1(qubit q) {}

def QDiscard(qubit q) {}

//
// Measurement.
//

def QMeas(qubit q) -> bit {
    bit c;
    c = measure q;
    reset q;
    return c;
}
