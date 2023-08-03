# Tests the decomposition of dag(sx).
gap> Read("Gates.g");;
gap> inv := m_x * m_sx;;
gap> m_sx * inv = QuditId( 2 );
true
