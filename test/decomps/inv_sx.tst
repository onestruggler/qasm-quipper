# Tests the decomposition of omega.
gap> Read("Gates.g");;
gap> inv := m_x * m_sx;;
gap> m_sx * inv = QuditId( 2 );
true
