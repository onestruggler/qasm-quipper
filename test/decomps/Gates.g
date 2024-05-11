Read("quantum-gap/src/Base.g");
Read("quantum-gap/src/Qudit.g");
Read("quantum-gap/src/Qubit.g");
Read("quantum-gap/src/CliffordT.g");

# Quipper parameter-free operators.
m_e := [ [ -1 + c_i, 1 + c_i  ],
         [ -1 + c_i, -1 - c_i ] ] / 2;
m_w := [ [ 1, 0,           0,            0 ],
         [ 0, 1 / c_sqrt2, 1 / c_sqrt2,  0 ],
         [ 0, 1 / c_sqrt2, -1 / c_sqrt2, 0 ],
         [ 0, 0,           0,            1 ] ];

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
