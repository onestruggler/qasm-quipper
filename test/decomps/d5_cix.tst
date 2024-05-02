# Tests the decomposition of C(iX).
gap> Read("Gates.g");;
gap> m_cix := AddQubitControl( m_ix );;
gap> m_tmp := ApplyQubitGateBetween( m_s, 0, 1 );;
gap> m_cix = m_tmp * m_cx;
true
