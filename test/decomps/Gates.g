Read("quantum-gap/src/Base.g");
Read("quantum-gap/src/Qudit.g");
Read("quantum-gap/src/Qubit.g");

# Constants (e.g., roots of unity).
c_omg   := E( 8 );
c_i     := E( 4 );
c_sqrt2 := ER( 2 );

# The global phase gate.
GPhase := function( p )
    return [ [ p, 0 ], [ 0, p ] ];
end;

# Common parameter-free operators.
m_x    := [ [ 0, 1 ],
            [ 1, 0 ] ];
m_y    := [ [ 0, -c_i ],
            [ c_i, 0 ] ];
m_z    := [ [ 1, 0 ],
            [ 0, -1 ] ];
m_sx   := [ [ 1 - c_i, 1 + c_i ],
            [ 1 + c_i, 1 - c_i ] ] / 2;
m_s    := [ [ 1, 0 ],
            [ 0, c_i ] ];
m_t    := [ [ 1, 0 ],
            [ 0, c_omg ] ];
m_sdg  := [ [ 1, 0 ],
            [ 0, -c_i ] ];
m_tdg  := [ [ 1, 0 ],
            [ 0, c_omg^7 ] ];
m_h    := [ [ 1, 1 ],
            [ 1, -1 ] ] / c_sqrt2;
m_swap := [ [ 1, 0, 0, 0 ],
            [ 0, 0, 1, 0 ],
            [ 0, 1, 0, 0 ],
            [ 0, 0, 0, 1 ] ];

# Derived operators.
m_cx    := AddQubitControl( m_x );
m_ccx   := AddQubitControl( m_cx );
m_cz    := AddQubitControl( m_z );
m_ch    := AddQubitControl( m_h );
m_cswap := AddQubitControl( m_swap );

# Quipper parameter-free operators.
m_e   := [ [ -1 + c_i, 1 + c_i ],
           [ -1 + c_i, -1 - c_i ] ] / 2;
m_w   := [ [ 1, 0,           0,            0 ],
           [ 0, 1 / c_sqrt2, 1 / c_sqrt2,  0 ],
           [ 0, 1 / c_sqrt2, -1 / c_sqrt2, 0 ],
           [ 0, 0,           0,            1 ] ];
m_ix  := c_i * m_x;
m_omg := GPhase( c_omg );

# Each PGate( x ) corresponds to P( log( x ) ) in OpenQASM.
PGate := function( x ) 
    return [ [ 1, 0 ], [ 0, x ] ];
end;

# Produces a list of matrix exponentials for testing gate decomposition tests.
# For each rotation angle t, the matrices e^(iMt), e^(iMt/2), and e^(-iMt/2)
# are returned.
SampleExps := function( M )
    local tmp1, tmp2, tmp3;

    tmp1 := 1 / Sqrt( 2 );
    tmp2 := 1 / 2;
    tmp3 := Sqrt( 3 ) / 2;

    return [ # [ theta = 0 ]
             [ SelfInvExp( 2, M, 1, 0 ),
               SelfInvExp( 2, M, 1, 0 ),
               SelfInvExp( 2, M, 1, 0 ) ],
             # [ theta = pi / 2 ]
             [ SelfInvExp( 2, M, 0, 1 ),
               SelfInvExp( 2, M, tmp1, tmp1 ),
               SelfInvExp( 2, M, tmp1, -tmp1 ) ],
             # [ theta = pi ]
             [ SelfInvExp( 2, M, -1, 0 ),
               SelfInvExp( 2, M, 0, 1 ),
               SelfInvExp( 2, M, 0, -1 ) ],
             # [ theta = 3 pi / 2 ]
             [ SelfInvExp( 2, M, 0, -1 ),
               SelfInvExp( 2, M, -tmp1, tmp1 ),
               SelfInvExp( 2, M, -tmp1, -tmp1 ) ],
             # [ theta = 2 pi / 3 ]
             [ SelfInvExp( 2, M, tmp2, tmp3 ),
               SelfInvExp( 2, M, tmp3, tmp2 ),
               SelfInvExp( 2, M, tmp3, -tmp2 ) ] ];
end;
