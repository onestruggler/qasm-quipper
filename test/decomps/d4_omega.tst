# Tests the decomposition of omega.
gap> Read("Gates.g");;
gap> inv := m_omg^7;;
gap> m_omg * inv = QuditId( 2 );
true
