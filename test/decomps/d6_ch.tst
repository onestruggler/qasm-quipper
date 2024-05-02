# Tests the decomposition of C(H).
gap> Read("Gates.g");;

# Uncontrolled gates.
gap> m_ih   := ApplyQubitGateBetween( m_h, 1, 0 );;
gap> m_is   := ApplyQubitGateBetween( m_s, 1, 0 );;
gap> m_it   := ApplyQubitGateBetween( m_t, 1, 0 );;
gap> m_isdg := ApplyQubitGateBetween( m_sdg, 1, 0 );;
gap> m_itdg := ApplyQubitGateBetween( m_tdg, 1, 0 );;

# The equation.
gap> m_ch = m_isdg * m_ih * m_itdg * m_cx * m_it * m_ih * m_is;
true
