# Tests the decomposition of C(Rx).

gap> Read("Gates.g");;
gap> x_rots := SampleExps( m_x );;
gap> z_rots := SampleExps( m_z );;
gap> m_ih   := ApplyQubitGateBetween( m_h, 1, 0 );;

# Test Case 1.
gap> j     := 1;;
gap> m_crx := AddQubitControl( x_rots[j][1] );;
gap> m_crz := AddQubitControl( z_rots[j][1] );;
gap> m_crx = m_ih * m_crz * m_ih;
true

# Test Case 2.
gap> j     := 2;;
gap> m_crx := AddQubitControl( x_rots[j][1] );;
gap> m_crz := AddQubitControl( z_rots[j][1] );;
gap> m_crx = m_ih * m_crz * m_ih;
true

# Test Case 3.
gap> j     := 3;;
gap> m_crx := AddQubitControl( x_rots[j][1] );;
gap> m_crz := AddQubitControl( z_rots[j][1] );;
gap> m_crx = m_ih * m_crz * m_ih;
true

# Test Case 4.
gap> j     := 4;;
gap> m_crx := AddQubitControl( x_rots[j][1] );;
gap> m_crz := AddQubitControl( z_rots[j][1] );;
gap> m_crx = m_ih * m_crz * m_ih;
true

# Test Case 5.
gap> j     := 5;;
gap> m_crx := AddQubitControl( x_rots[j][1] );;
gap> m_crz := AddQubitControl( z_rots[j][1] );;
gap> m_crx = m_ih * m_crz * m_ih;
true
