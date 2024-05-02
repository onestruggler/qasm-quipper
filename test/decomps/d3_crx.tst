# Tests the decomposition of C(Rx).

gap> Read("Gates.g");;
gap> mexps := SampleExps( m_x );;

# Test Case 1.
gap> j     := 1;;
gap> case  := mexps[j];;
gap> m_crx := AddQubitControl( case[1] );;
gap> m_rx1 := ApplyQubitGateBetween( case[2], 1, 0 );;
gap> m_rx2 := ApplyQubitGateBetween( case[3], 1, 0 );;
gap> m_crx = m_cz * m_rx2 * m_cz * m_rx1;
true

# Test Case 2.
gap> j     := 2;;
gap> case  := mexps[j];;
gap> m_crx := AddQubitControl( case[1] );;
gap> m_rx1 := ApplyQubitGateBetween( case[2], 1, 0 );;
gap> m_rx2 := ApplyQubitGateBetween( case[3], 1, 0 );;
gap> m_crx = m_cz * m_rx2 * m_cz * m_rx1;
true

# Test Case 3.
gap> j     := 3;;
gap> case  := mexps[j];;
gap> m_crx := AddQubitControl( case[1] );;
gap> m_rx1 := ApplyQubitGateBetween( case[2], 1, 0 );;
gap> m_rx2 := ApplyQubitGateBetween( case[3], 1, 0 );;
gap> m_crx = m_cz * m_rx2 * m_cz * m_rx1;
true

# Test Case 4.
gap> j     := 4;;
gap> case  := mexps[j];;
gap> m_crx := AddQubitControl( case[1] );;
gap> m_rx1 := ApplyQubitGateBetween( case[2], 1, 0 );;
gap> m_rx2 := ApplyQubitGateBetween( case[3], 1, 0 );;
gap> m_crx = m_cz * m_rx2 * m_cz * m_rx1;
true

# Test Case 5.
gap> j     := 5;;
gap> case  := mexps[j];;
gap> m_crx := AddQubitControl( case[1] );;
gap> m_rx1 := ApplyQubitGateBetween( case[2], 1, 0 );;
gap> m_rx2 := ApplyQubitGateBetween( case[3], 1, 0 );;
gap> m_crx = m_cz * m_rx2 * m_cz * m_rx1;
true
