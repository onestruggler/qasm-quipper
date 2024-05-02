# Tests the decomposition of C(W).
gap> Read("Gates.g");;

# Computes permutationss of cx.
gap> m_cxi := ApplyQubitGateBetween( m_cx, 0, 1 );;
gap> m_icx := ApplyQubitGateBetween( m_cx, 1, 0 );;
gap> m_cix := QubitSwapAndApply( 1, 2, m_icx );;
gap> m_xic := QubitSwapAndApply( 1, 3, m_cix );;

# Validates decomposition.
gap> m_cw := AddQubitControl( m_w );;
gap> m_a1 := m_t * m_h * m_tdg * m_h * m_sdg;;
gap> m_a2 := KroneckerProduct( m_a1, m_t );;
gap> m_a3 := m_a2 * m_cx;;
gap> m_g1 := KroneckerProduct( m_t, m_a3 );;
gap> m_b1 := KroneckerProduct( m_t, m_tdg );;
gap> m_b2 := ApplyQubitGateBetween( m_b1, 1, 0 );;
gap> m_g2 := m_cix * m_b2 * m_cxi * m_icx * m_xic;;
gap> m_c1 := KroneckerProduct( m_tdg, m_tdg );;
gap> m_c2 := ApplyQubitGateBetween( m_c1, 0, 1 );;
gap> m_c3 := QubitSwapAndApply( 2, 3, m_c2 );;
gap> m_g3 := m_xic * m_cxi * m_icx * m_c3;;
gap> m_d1 := m_s * m_h * m_t * m_h;;
gap> m_d2 := ApplyQubitGateBetween( m_d1, 1, 1 );;
gap> m_g4 := m_icx * m_d2;;
gap> m_cw = m_g4 * m_g3 * m_g2 * m_g1;
true
