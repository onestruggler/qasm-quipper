# Tests the decomposition of C(T).
gap> Read("Gates.g");;

# Computes permutates of cx.
gap> m_xc  := SwapAndApply( 2, 2, 1, 2, m_cx );;
gap> m_xci := ApplyQuditGateBetween( 2, m_xc, 0, 1 );;
gap> m_ixc := ApplyQuditGateBetween( 2, m_xc, 1, 0 );;
gap> m_xic := SwapAndApply( 2, 3, 2, 3, m_xci );;

# Validates decomposition.
gap> m_ct   := AddQubitControl( m_t );;
gap> m_a1   := ApplyQuditGateBetween( 2, m_h, 2, 0 );;
gap> m_g1   := m_xci * m_ixc * m_a1;;
gap> m_b1   := KroneckerProduct( m_tdg, m_t );;
gap> m_b2   := ApplyQuditGateBetween( 2, m_b1, 0, 1 );;
gap> m_g2   := m_xci * m_ixc * m_b2;;
gap> m_c1   := KroneckerProduct( m_t, m_tdg );;
gap> m_c2   := ApplyQuditGateBetween( 2, m_c1, 0, 1 );;
gap> m_c3   := SwapAndApply( 2, 3, 2, 3, m_c2 );;
gap> m_c4   := SwapAndApply( 2, 3, 1, 3, m_c3 );;
gap> m_g3   := m_xic * m_c3;;
gap> m_d1   := m_h * m_t * m_h;;
gap> m_d2   := ApplyQuditGateBetween( 2, m_d1, 2, 0 );;
gap> m_g4   := m_ixc * m_xci * m_c2 * m_ixc * m_xci * m_c4 * m_xic * m_d2;;
gap> m_g5   := ApplyQuditGateBetween( 2, m_h, 2, 0 );;
gap> decomp := m_g5 * m_g4 * m_g3 * m_g2 * m_g1;;
gap> m_ct = AssertAncilla( 2, decomp, 0 );
true

# Validates ancilla usage.
gap> CheckAncilla( 2, decomp, 0 );
true
