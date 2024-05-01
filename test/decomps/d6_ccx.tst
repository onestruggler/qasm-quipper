# Tests the decomposition of C(W).
gap> Read("Gates.g");;

# Base gates.
gap> m_cxiiiii := ApplyQuditGateBetween( 2, m_cx, 0, 5 );;
gap> m_iihiiii := ApplyQuditGateBetween( 2, m_h, 2, 4 );;

# Computes permutationss of cx.
gap> m_ciixiii := SwapAndApply( 2, 2, 4, m_cxiiiii );;
gap> m_iiixici := SwapAndApply( 2, 1, 6, m_ciixiii );;
gap> m_ciiixii := SwapAndApply( 2, 2, 5, m_cxiiiii );;
gap> m_iciixii := SwapAndApply( 2, 1, 2, m_ciiixii );;
gap> m_iciiixi := SwapAndApply( 2, 5, 6, m_iciixii );;
gap> m_iiciixi := SwapAndApply( 2, 2, 3, m_iciiixi );;
gap> m_iiciiix := SwapAndApply( 2, 6, 7, m_iiciixi );;
gap> m_iiiciix := SwapAndApply( 2, 3, 4, m_iiciiix );;

# Central component.
gap> m_tt        := KroneckerProduct( m_t, m_t );;
gap> m_tttt      := KroneckerProduct( m_tt, m_tt );;
gap> m_tdgtdg    := KroneckerProduct( m_tdg, m_tdg );;
gap> m_tdgtdgtdg := KroneckerProduct( m_tdgtdg, m_tdg );;
gap> m_mid       := KroneckerProduct( m_tttt, m_tdgtdgtdg );;

# Full circuit, with ancilla.
gap> m_lhs       := m_iciiixi * m_ciixiii * m_iciixii * m_iiciixi * m_iiiciix * m_ciiixii * m_iiciiix * m_iiixici;;
gap> m_rhs       := m_iiixici * m_iiciiix * m_ciiixii * m_iiiciix * m_iiciixi * m_iciixii * m_ciixiii * m_iciiixi;;
gap> m_circ_4anc := m_iihiiii * m_lhs * m_mid * m_rhs * m_iihiiii;;

# Expands first layer of ancilla.
gap> m_circ_3anc := AssertAncilla( 2, m_circ_4anc, 0 );;
gap> CheckAncilla( 2, m_circ_4anc, 0 );
true

# Expands second layer of ancilla.
gap> m_circ_2anc := AssertAncilla( 2, m_circ_3anc, 0 );;
gap> CheckAncilla( 2, m_circ_3anc, 0 );
true

# Expands third layer of ancilla.
gap> m_circ_1anc := AssertAncilla( 2, m_circ_2anc, 0 );;
gap> CheckAncilla( 2, m_circ_2anc, 0 );
true

# Expands fourth layer of ancilla.
gap> m_circ := AssertAncilla( 2, m_circ_1anc, 0 );;
gap> CheckAncilla( 2, m_circ_1anc, 0 );
true

# Validates the circuit.
gap> m_circ = m_ccx;
true
