# Tests the decomposition of omega.
gap> Read("Gates.g");;
gap> inv := m_omg^7;;
gap> m_omg * inv = QutritId( 2 );
true
