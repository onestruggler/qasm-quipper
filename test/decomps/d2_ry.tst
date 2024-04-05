# Tests the decomposition of Ry.

gap> Read("Gates.g");;
gap> y_rots := SampleExps( m_y );;
gap> z_rots := SampleExps( m_z );;

# Test Case 1.
gap> j := 1;;
gap> y_rots[j][1] = m_x * m_sdg * m_h * z_rots[j][1] * m_h * m_s * m_x;
true

# Test Case 2.
gap> j := 2;;
gap> y_rots[j][1] = m_x * m_sdg * m_h * z_rots[j][1] * m_h * m_s * m_x;
true

# Test Case 3.
gap> j := 3;;
gap> y_rots[j][1] = m_x * m_sdg * m_h * z_rots[j][1] * m_h * m_s * m_x;
true

# Test Case 4.
gap> j := 4;;
gap> y_rots[j][1] = m_x * m_sdg * m_h * z_rots[j][1] * m_h * m_s * m_x;
true

# Test Case 5.
gap> j := 5;;
gap> y_rots[j][1] = m_x * m_sdg * m_h * z_rots[j][1] * m_h * m_s * m_x;
true
