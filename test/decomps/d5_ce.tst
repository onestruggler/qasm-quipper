# Tests the decomposition of C(E).
gap> Read("Gates.g");;
gap> m_ce := AddQubitControl( m_e );;
gap> m_xc := QubitSwapAndApply( 1, 2, m_cx );;
gap> m_g1 := KroneckerProduct( m_t, m_tdg );;
gap> m_g2 := ApplyQubitGateBetween( m_h * m_tdg, 1, 0 );;
gap> m_g3 := KroneckerProduct( m_s, m_t * m_h );;
gap> m_ce = m_xc * m_g1 * m_xc * m_g2 * m_cx * m_g3;
true
