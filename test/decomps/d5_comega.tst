# Tests the decomposition of C(omega).
gap> Read("Gates.g");;
gap> m_comg := AddQubitControl( m_omg );;
gap> m_ppi4 := PGate( E( 8 ) );;
gap> m_comg = ApplyQubitGateBetween( m_ppi4, 0, 1 );
true
