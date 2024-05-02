# Tests the decomposition of C(SWAP).
gap> Read("Gates.g");;
gap> m_xc  := QubitSwapAndApply( 1, 2, m_cx );;
gap> m_cxc := QubitSwapAndApply( 2, 3, m_ccx );;
gap> m_icx := ApplyQubitGateBetween( m_cx, 1, 0 );;
gap> m_cswap = m_icx * m_cxc * m_icx;
true
