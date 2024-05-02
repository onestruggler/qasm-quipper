# Tests the decomposition of C(Ry).

gap> Read("Gates.g");;
gap> y_rots := SampleExps( m_y );;
gap> z_rots := SampleExps( m_z );;
gap> m_ix   := ApplyQubitGateBetween( m_x, 1, 0 );;
gap> m_is   := ApplyQubitGateBetween( m_s, 1, 0 );;
gap> m_isdg := ApplyQubitGateBetween( m_sdg, 1, 0 );;
gap> m_ih   := ApplyQubitGateBetween( m_h, 1, 0 );;

# Test Case 1.
gap> j     := 1;;
gap> m_cry := AddQubitControl( y_rots[j][1] );;
gap> m_crz := AddQubitControl( z_rots[j][1] );;
gap> m_cry = m_ix * m_isdg * m_ih * m_crz * m_ih * m_is * m_ix;
true

# Test Case 2.
gap> j     := 2;;
gap> m_cry := AddQubitControl( y_rots[j][1] );;
gap> m_crz := AddQubitControl( z_rots[j][1] );;
gap> m_cry = m_ix * m_isdg * m_ih * m_crz * m_ih * m_is * m_ix;
true

# Test Case 3.
gap> j     := 3;;
gap> m_cry := AddQubitControl( y_rots[j][1] );;
gap> m_crz := AddQubitControl( z_rots[j][1] );;
gap> m_cry = m_ix * m_isdg * m_ih * m_crz * m_ih * m_is * m_ix;
true

# Test Case 4.
gap> j     := 4;;
gap> m_cry := AddQubitControl( y_rots[j][1] );;
gap> m_crz := AddQubitControl( z_rots[j][1] );;
gap> m_cry = m_ix * m_isdg * m_ih * m_crz * m_ih * m_is * m_ix;
true

# Test Case 5.
gap> j     := 5;;
gap> m_cry := AddQubitControl( y_rots[j][1] );;
gap> m_crz := AddQubitControl( z_rots[j][1] );;
gap> m_cry = m_ix * m_isdg * m_ih * m_crz * m_ih * m_is * m_ix;
true
