# Tests the decomposition of W.
gap> Read("Gates.g");;
gap> m_xc := m_swap * m_cx * m_swap;;
gap> m_x2 := ApplyQubitGateBetween( m_x, 1, 0 );;
gap> m_w = m_xc * m_x2 * m_ch * m_x2 * m_xc;
true
