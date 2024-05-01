# Tests the decomposition of C(W).
gap> Read("Gates.g");;

# Base gates.
gap> m_xc  := SwapAndApply( 2, 1, 2, m_cx );;
gap> m_iih := ApplyQuditGateBetween( 2, m_h, 2, 0 );;

# Computes permutationss of cx.
gap> m_cxi := ApplyQuditGateBetween( 2, m_cx, 0, 1 );;
gap> m_xci := ApplyQuditGateBetween( 2, m_xc, 0, 1 );;
gap> m_ixc := ApplyQuditGateBetween( 2, m_xc, 1, 0 );;
gap> m_cix := SwapAndApply( 2, 2, 3, m_cxi );;

# Constructs T layers.
gap> m_tt     := KroneckerProduct( m_t, m_t );;
gap> m_tl1    := KroneckerProduct( m_t, m_tt );;
gap> m_tdgt   := KroneckerProduct( m_tdg, m_t );;
gap> m_tl2    := ApplyQuditGateBetween( 2, m_tdgt, 1, 0 );;
gap> m_tdgtdg := KroneckerProduct( m_tdg, m_tdg );;
gap> m_tl3    := ApplyQuditGateBetween( 2, m_tdgtdg, 0, 1 );;

# Constructs cnot layers.
gap> m_xl1 := m_iih * m_ixc;;
gap> m_xl2 := m_cix * m_ixc * m_xci;;
gap> m_xl3 := m_cxi;;
gap> m_xl4 := m_ixc * m_iih * m_xci * m_cix * m_ixc;;

# Full circuit with ancilla.
gap> m_circ := m_xl4 * m_tl3 * m_xl3 * m_tl2 * m_xl2 * m_tl1 * m_xl1;;
gap> m_cswap = m_circ;
true
