# Tests the decomposition of C(Ry).

gap> Read("Gates.g");;
gap> mexps := SampleExps( m_y );;

# Test Case 1.
gap> j     := 1;;
gap> case  := mexps[j];;
gap> m_cry := AddQubitControl( case[1] );;
gap> m_ry1 := ApplyQuditGateBetween( 2, case[2], 1, 0 );;
gap> m_ry2 := ApplyQuditGateBetween( 2, case[3], 1, 0 );;
gap> m_cry = m_cz * m_ry2 * m_cz * m_ry1;
true

# Test Case 2.
gap> j     := 2;;
gap> case  := mexps[j];;
gap> m_cry := AddQubitControl( case[1] );;
gap> m_ry1 := ApplyQuditGateBetween( 2, case[2], 1, 0 );;
gap> m_ry2 := ApplyQuditGateBetween( 2, case[3], 1, 0 );;
gap> m_cry = m_cz * m_ry2 * m_cz * m_ry1;
true

# Test Case 3.
gap> j     := 3;;
gap> case  := mexps[j];;
gap> m_cry := AddQubitControl( case[1] );;
gap> m_ry1 := ApplyQuditGateBetween( 2, case[2], 1, 0 );;
gap> m_ry2 := ApplyQuditGateBetween( 2, case[3], 1, 0 );;
gap> m_cry = m_cz * m_ry2 * m_cz * m_ry1;
true

# Test Case 4.
gap> j     := 4;;
gap> case  := mexps[j];;
gap> m_cry := AddQubitControl( case[1] );;
gap> m_ry1 := ApplyQuditGateBetween( 2, case[2], 1, 0 );;
gap> m_ry2 := ApplyQuditGateBetween( 2, case[3], 1, 0 );;
gap> m_cry = m_cz * m_ry2 * m_cz * m_ry1;
true

# Test Case 5.
gap> j     := 5;;
gap> case  := mexps[j];;
gap> m_cry := AddQubitControl( case[1] );;
gap> m_ry1 := ApplyQuditGateBetween( 2, case[2], 1, 0 );;
gap> m_ry2 := ApplyQuditGateBetween( 2, case[3], 1, 0 );;
gap> m_cry = m_cz * m_ry2 * m_cz * m_ry1;
true
