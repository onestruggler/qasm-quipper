# Tests the decomposition of C(SWAP).
gap> Read("Gates.g");;
gap> m_xc  := SwapAndApply( 2, 1, 2, m_cx );;
gap> m_cxc := SwapAndApply( 2, 2, 3, m_ccx );;
gap> m_icx := ApplyQuditGateBetween( 2, m_cx, 1, 0 );;
gap> m_cswap = m_icx * m_cxc * m_icx;
true
