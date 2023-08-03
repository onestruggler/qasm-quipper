# Tests the decomposition of C(P).

gap> Read("Gates.g");;

# Test Case 1.
gap> m_cp := AddQubitControl( PGate( 1 ) );;
gap> m_p1 := PGate( 1 );;
gap> m_p2 := PGate( 1 );;
gap> m_p3 := ApplyQuditGateBetween( 2, m_p1, 1, 0 );;
gap> m_p4 := ApplyQuditGateBetween( 2, m_p2, 1, 0 );;
gap> m_p5 := ApplyQuditGateBetween( 2, m_p1, 0, 1 );;
gap> m_cp = m_p5 * m_cx * m_p4 * m_cx * m_p3;
true

# Test Case 2.
gap> m_cp := AddQubitControl( PGate( E(4)^2 ) );;
gap> m_p1 := PGate( E(4) );;
gap> m_p2 := PGate( E(4)^3 );;
gap> m_p3 := ApplyQuditGateBetween( 2, m_p1, 1, 0 );;
gap> m_p4 := ApplyQuditGateBetween( 2, m_p2, 1, 0 );;
gap> m_p5 := ApplyQuditGateBetween( 2, m_p1, 0, 1 );;
gap> m_cp = m_p5 * m_cx * m_p4 * m_cx * m_p3;
true

# Test Case 3.
gap> m_cp := AddQubitControl( PGate( E(5)^2 ) );;
gap> m_p1 := PGate( E(5) );;
gap> m_p2 := PGate( E(5)^4 );;
gap> m_p3 := ApplyQuditGateBetween( 2, m_p1, 1, 0 );;
gap> m_p4 := ApplyQuditGateBetween( 2, m_p2, 1, 0 );;
gap> m_p5 := ApplyQuditGateBetween( 2, m_p1, 0, 1 );;
gap> m_cp = m_p5 * m_cx * m_p4 * m_cx * m_p3;
true

# Test Case 4.
gap> m_cp := AddQubitControl( PGate( E(6)^2 ) );;
gap> m_p1 := PGate( E(6) );;
gap> m_p2 := PGate( E(6)^5 );;
gap> m_p3 := ApplyQuditGateBetween( 2, m_p1, 1, 0 );;
gap> m_p4 := ApplyQuditGateBetween( 2, m_p2, 1, 0 );;
gap> m_p5 := ApplyQuditGateBetween( 2, m_p1, 0, 1 );;
gap> m_cp = m_p5 * m_cx * m_p4 * m_cx * m_p3;
true

# Test Case 5.
gap> m_cp := AddQubitControl( PGate( E(7)^2 ) );;
gap> m_p1 := PGate( E(7) );;
gap> m_p2 := PGate( E(7)^6 );;
gap> m_p3 := ApplyQuditGateBetween( 2, m_p1, 1, 0 );;
gap> m_p4 := ApplyQuditGateBetween( 2, m_p2, 1, 0 );;
gap> m_p5 := ApplyQuditGateBetween( 2, m_p1, 0, 1 );;
gap> m_cp = m_p5 * m_cx * m_p4 * m_cx * m_p3;
true
