# Tests the decomposition of C(T).
gap> Read("Gates.g");;

# Computes permutations of cx.
gap> m_xc  := QubitSwapAndApply( 1, 2, m_cx );;
gap> m_xci := ApplyQubitGateBetween( m_xc, 0, 1 );;
gap> m_ixc := ApplyQubitGateBetween( m_xc, 1, 0 );;
gap> m_xic := QubitSwapAndApply( 2, 3, m_xci );;

# Validates decomposition.
gap> m_ct   := AddQubitControl( m_t );;
gap> m_a1   := ApplyQubitGateBetween( m_h, 2, 0 );;
gap> m_g1   := m_xci * m_ixc * m_a1;;
gap> m_b1   := KroneckerProduct( m_tdg, m_t );;
gap> m_b2   := ApplyQubitGateBetween( m_b1, 0, 1 );;
gap> m_g2   := m_xci * m_ixc * m_b2;;
gap> m_c1   := KroneckerProduct( m_t, m_tdg );;
gap> m_c2   := ApplyQubitGateBetween( m_c1, 0, 1 );;
gap> m_c3   := QubitSwapAndApply( 2, 3, m_c2 );;
gap> m_c4   := QubitSwapAndApply( 1, 3, m_c3 );;
gap> m_g3   := m_xic * m_c3;;
gap> m_d1   := m_h * m_t * m_h;;
gap> m_d2   := ApplyQubitGateBetween( m_d1, 2, 0 );;
gap> m_g4   := m_ixc * m_xci * m_c2 * m_ixc * m_xci * m_c4 * m_xic * m_d2;;
gap> m_g5   := ApplyQubitGateBetween( m_h, 2, 0 );;
gap> decomp := m_g5 * m_g4 * m_g3 * m_g2 * m_g1;;
gap> m_ct = AssertQubitAncilla( decomp );
true

# Validates ancilla usage.
gap> CheckQubitAncilla( decomp );
true
