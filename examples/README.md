# Example Programs

The directory provides example programs to demo translation feautres.
- **Quantum Fourier Transform**: The `qft` directory provides three
  implementations of the quantum Fourier transform. The files `qft.2.qasm` and
  `qft.3.qasm` provide implementations in OpenQASM 2.0 and OpenQASM 3. The file
  `qft.quip` provides an implementation in Quipper. The three implementations
  highlight the input formats supported by the translation pipeline.
- **Quantum Phase Estimation**: The `qpe` directory provides two implementation
  of quantum phase estimation. The file `qpe.3.qasm` provides an implementation
  in OpenQASM 3, whereas the file `qpe.quip` provides an implementation in
  Quipper. Th two implementations highlight how ancilla qubits and measurements
  impact translations.