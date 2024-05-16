# Tests the decomposition of CC(Z).
gap> Read("Gates.g");;

# Base gates.
gap> m_cxiiiii := ApplyQubitGateBetween( m_cx, 0, 5 );;

# Computes permutationss of cx.
gap> m_ciixiii := QubitSwapAndApply( 2, 4, m_cxiiiii );;
gap> m_iiixici := QubitSwapAndApply( 1, 6, m_ciixiii );;
gap> m_ciiixii := QubitSwapAndApply( 2, 5, m_cxiiiii );;
gap> m_iciixii := QubitSwapAndApply( 1, 2, m_ciiixii );;
gap> m_iciiixi := QubitSwapAndApply( 5, 6, m_iciixii );;
gap> m_iiciixi := QubitSwapAndApply( 2, 3, m_iciiixi );;
gap> m_iiciiix := QubitSwapAndApply( 6, 7, m_iiciixi );;
gap> m_iiiciix := QubitSwapAndApply( 3, 4, m_iiciiix );;

# Central component.
gap> m_tt        := KroneckerProduct( m_t, m_t );;
gap> m_tttt      := KroneckerProduct( m_tt, m_tt );;
gap> m_tdgtdg    := KroneckerProduct( m_tdg, m_tdg );;
gap> m_tdgtdgtdg := KroneckerProduct( m_tdgtdg, m_tdg );;
gap> m_mid       := KroneckerProduct( m_tttt, m_tdgtdgtdg );;

# Full circuit, with ancilla.
gap> m_lhs       := m_iciiixi * m_ciixiii * m_iciixii * m_iiciixi * m_iiiciix * m_ciiixii * m_iiciiix * m_iiixici;;
gap> m_rhs       := m_iiixici * m_iiciiix * m_ciiixii * m_iiiciix * m_iiciixi * m_iciixii * m_ciixiii * m_iciiixi;;
gap> m_circ_4anc := m_lhs * m_mid * m_rhs;;

# Expands first layer of ancilla.
gap> m_circ_3anc := AssertQubitAncilla( m_circ_4anc );;
gap> CheckQubitAncilla( m_circ_4anc );
true

# Expands second layer of ancilla.
gap> m_circ_2anc := AssertQubitAncilla( m_circ_3anc );;
gap> CheckQubitAncilla( m_circ_3anc );
true

# Expands third layer of ancilla.
gap> m_circ_1anc := AssertQubitAncilla( m_circ_2anc );;
gap> CheckQubitAncilla( m_circ_2anc );
true

# Expands fourth layer of ancilla.
gap> m_circ := AssertQubitAncilla( m_circ_1anc );;
gap> CheckQubitAncilla( m_circ_1anc );
true

# Validates the circuit.
gap> m_circ = m_ccz;
true
