Read("quantum-gap/src/Base.g");
Read("quantum-gap/src/Qutrit.g");
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
m_sx   := [ [ 1 - c_i, 1 + c_i ],
            [ 1 + c_i, 1 - c_i ] ] / 2;
m_s    := [ [ 1, 0 ],
            [ 0, c_i ] ];
m_h    := [ [ 1, 1 ],
            [ 1, -1 ] ] / c_sqrt2;
m_swap := [ [ 1, 0, 0, 0 ],
            [ 0, 0, 1, 0 ],
            [ 0, 1, 0, 0 ],
            [ 0, 0, 0, 1 ] ];

# Derived operators.
m_cx := AddQubitControl( m_x );
m_ch := AddQubitControl( m_h );

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

