# Tests the decomposition of C(S).
gap> Read("Gates.g");;
gap> m_cs := AddQubitControl( m_s );;
gap> m_xc := QubitSwapAndApply( 1, 2, m_cx );;
gap> m_g1 := ApplyQubitGateBetween( m_tdg, 0, 1 );;
gap> m_g2 := KroneckerProduct( m_t, m_t );;
gap> m_cs = m_xc * m_g1 * m_xc * m_g2;
true
