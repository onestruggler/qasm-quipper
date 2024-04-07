# Tests the decomposition of C(H).
gap> Read("Gates.g");;

# Uncontrolled gates.
gap> m_ih   := ApplyQuditGateBetween( 2, m_h, 1, 0 );;
gap> m_is   := ApplyQuditGateBetween( 2, m_s, 1, 0 );;
gap> m_it   := ApplyQuditGateBetween( 2, m_t, 1, 0 );;
gap> m_isdg := ApplyQuditGateBetween( 2, m_sdg, 1, 0 );;
gap> m_itdg := ApplyQuditGateBetween( 2, m_tdg, 1, 0 );;

# The equation.
gap> m_ch = m_isdg * m_ih * m_itdg * m_cx * m_it * m_ih * m_is;
true
