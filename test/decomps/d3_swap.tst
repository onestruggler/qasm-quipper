# Tests the decomposition of SWAP.
gap> Read("Gates.g");;
gap> m_xc := QubitSwapAndApply( 1, 2, m_cx );;
gap> m_swap = m_cx * m_xc * m_cx;
true
