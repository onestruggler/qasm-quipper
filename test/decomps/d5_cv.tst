# Tests the decomposition of C(V).
gap> Read("Gates.g");;
gap> m_cv := AddQubitControl( m_sx );;
gap> m_xc := QubitSwapAndApply( 1, 2, m_cx );;
gap> m_g1 := ApplyQubitGateBetween( m_h, 1, 0 );;
gap> m_g2 := KroneckerProduct( m_t, m_tdg );;
gap> m_g3 := KroneckerProduct( m_tdg, m_h );;
gap> m_cv = m_g1 * m_xc * m_g2 * m_xc * m_g3;
true
