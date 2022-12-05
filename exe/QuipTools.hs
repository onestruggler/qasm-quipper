{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Quipper
import Quipper.Libraries.QuipperASCIIParser

data IRGate b = IRGate b

data IR a = IROutput a
          | forall b. IROp (IRGate b) (IR a)

--my_transformer' :: Transformer [] Int Int
--my_transformer' (T_QGate str _ _ _ _ f) = f $

my_tansformer :: Transformer IO Qubit Bit
my_tansformer (T_QGate str _ _ _ _ f) = f $
    \qbits cbits ctrls -> do
        putStrLn str
        return (qbits, cbits, ctrls)

fn :: Int -> [Int]
fn x = [x, 10 * x, 100 * x]

mfn :: Int -> [Int]
mfn x = do
    t1 <- fn x
    t2 <- fn t1
    fn t2

main :: IO ()
main = do
    text <- getContents
    let (shape, circ) = parse_circuit text
    let f = transform_generic_shape my_tansformer circ shape
    _ <- f shape
    putStrLn (show $ mfn 5)
