# Tests the decomposition of dag(E).
gap> Read("Gates.g");;
gap> inv := m_s * m_h * m_omg^5;;
gap> m_e * inv = QuditId( 2 );
true
