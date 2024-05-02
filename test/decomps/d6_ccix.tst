# Tests the decomposition of C(W).
gap> Read("Gates.g");;

# Base gates.
gap> m_xc   := QubitSwapAndApply( 1, 2, m_cx );;
gap> m_cxii := ApplyQubitGateBetween( m_cx, 0, 2 );;
gap> m_iihi := ApplyQubitGateBetween( m_h, 2, 1 );;

# Computes permutationss of cx.
gap> m_ciix := QubitSwapAndApply( 2, 4, m_cxii );;
gap> m_icix := QubitSwapAndApply( 1, 2, m_ciix );;
gap> m_ixci := ApplyQubitGateBetween( m_xc, 1, 1 );;
gap> m_xici := QubitSwapAndApply( 1, 2, m_ixci );;

# Central component.
gap> m_tt     := KroneckerProduct( m_t, m_t );;
gap> m_tdgtdg := KroneckerProduct( m_tdg, m_tdg );;
gap> m_mid    := KroneckerProduct( m_tt, m_tdgtdg );;

# Full circuit, with ancilla.
gap> m_lhs      := m_ciix * m_ixci * m_xici * m_icix;;
gap> m_rhs      := m_icix * m_xici * m_ixci * m_ciix;;
gap> m_circ_anc := m_iihi * m_lhs * m_mid * m_rhs * m_iihi;;

# Expands layer of ancilla.
gap> m_circ := AssertQubitAncilla( m_circ_anc );;
gap> CheckQubitAncilla( m_circ_anc );
true

# Validates the circuit.
gap> m_expt := AddQubitControl( AddQubitControl( m_ix ) );;
gap> m_circ = m_expt;
true
