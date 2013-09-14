module Grammar where

import Prelude hiding(any)
import Any
import Control.Applicative((<$>), (<*>), liftA)
import Data.Function(on)

data S = Start B C W
data W = End E | Middle E W
data E = Rule A B C
data A = NonTerminal Char
data B = A A | Terminal Char
data C = C B C | Epsilon 

instance Show S where
    show (Start b c w) = "S <- " ++ show b ++ show c ++ "\n" ++ show w

instance Any S where
    any = Start <$> any <*> any <*> any

instance Show W where
    show (End e) = show e
    show (Middle e w) = show e ++ "\n" ++ show w

instance Any W where
    any = choose (End <$> any) (Middle <$> any <*> any)

instance Show E where
    show (Rule a b c) = show a ++ " <- " ++ show b ++ show c

instance Any E where
    any = Rule <$> any <*> any <*> any

instance Show A where
    show (NonTerminal c) = [c]

instance Any A where
    any = NonTerminal <$> toEnum <$> (+65) <$> (`mod` 26) <$> (any :: Gen Int)

instance Show B where
    show (A a) = show a
    show (Terminal c) = [c]

instance Any B where
    any = choose (A <$> any)
          (Terminal <$> toEnum <$>
               choose ((+32)   <$> (`mod` 33) <$> any) 
                      ((+1040) <$> (`mod` 64) <$> any))

instance Show C where
    show (C b c) = show b ++ show c
    show Epsilon = ""

instance Any C where
    any = do
        flag <- any
        if flag
          then C <$> any <*> any
          else return Epsilon
