module Main where

import Prelude hiding (any)
import Grammar 
import Any
import Control.Applicative((<$>))
import Control.Monad(void)
import qualified System.Random as R

main :: IO ()
main = void ((genSome :: IO S) >>= print)

genSome :: Any a => IO a
genSome = generate any <$> R.newStdGen
