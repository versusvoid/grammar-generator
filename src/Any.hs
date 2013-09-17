{-# LANGUAGE TupleSections #-}
module Any(
    Gen,
    Any,
    any,
    (<?>),
    runGen
    ) where

import           Prelude hiding(any)
import           Control.Applicative
import           Control.Monad
import           Control.Arrow(first)
import qualified System.Random as R

newtype Gen a = Gen { step :: R.StdGen -> (a,R.StdGen) }

instance Monad Gen where
    return a = Gen (a,)
    (Gen gf1) >>= f = 
        Gen (\g ->
            let (v1,g') = gf1 g
                Gen gf2 = f v1
            in gf2 g')

instance Functor Gen where
    fmap f (Gen gf) = Gen ((first f) . gf)

instance Applicative Gen where
    pure = return
    (<*>) = ap

class Any a where
    any :: Gen a

instance Any Int where
    any = Gen R.random

instance Any Bool where
    any = Gen R.random

(<?>) :: Gen a -> Gen a -> Gen a
gf1 <?> gf2 = do
    flag <- any
    if flag
      then gf1
      else gf2

runGen :: Gen a -> R.StdGen -> a
runGen (Gen gf) g = fst $ gf g
{-
instance R.Random a => Any a where
    any = return . fst . R.random
    -}
        
