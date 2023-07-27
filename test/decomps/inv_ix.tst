# Tests the decomposition of omega.
gap> Read("Gates.g");;
gap> inv := -m_ix;;
gap> m_ix * inv = QutritId( 2 );
true
