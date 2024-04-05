# Tests the decomposition of Rx.

gap> Read("Gates.g");;
gap> x_rots := SampleExps( m_x );;
gap> z_rots := SampleExps( m_z );;

# Test Case 1.
gap> j := 1;;
gap> z_rots[j][1] = m_h * x_rots[j][1] * m_h;
true

# Test Case 2.
gap> j := 2;;
gap> z_rots[j][1] = m_h * x_rots[j][1] * m_h;
true

# Test Case 3.
gap> j := 3;;
gap> z_rots[j][1] = m_h * x_rots[j][1] * m_h;
true

# Test Case 4.
gap> j := 4;;
gap> z_rots[j][1] = m_h * x_rots[j][1] * m_h;
true

# Test Case 5.
gap> j := 5;;
gap> z_rots[j][1] = m_h * x_rots[j][1] * m_h;
true
