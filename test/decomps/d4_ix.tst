# Tests the decomposition of dag(iX).
gap> Read("Gates.g");;
gap> inv := -m_ix;;
gap> m_ix * inv = QuditId( 2 );
true
