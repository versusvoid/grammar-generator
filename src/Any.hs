{-# LANGUAGE TupleSections #-}
module Any where

import           Prelude hiding(any)
import           Control.Applicative
import           Control.Monad
import qualified System.Random as R

newtype Gen a = Gen { generate :: R.StdGen -> a }

instance Monad Gen where
    return = Gen . const
    (Gen gf1) >>= f = 
        Gen (\g -> 
            let (g1,g2) = R.split g
                Gen gf2 = f $ gf1 g1
            in gf2 g2)

instance Functor Gen where
    fmap f (Gen gf) = Gen (f . gf)

instance Applicative Gen where
    pure = return
    (<*>) = ap

class Any a where
    any :: Gen a

instance Any Int where
    any = Gen (fst . R.random)

instance Any Bool where
    any = Gen (fst . R.random)

choose :: Gen a -> Gen a -> Gen a
choose gf1 gf2 = do
    flag <- any
    if flag
      then gf1
      else gf2

{-
instance R.Random a => Any a where
    any = return . fst . R.random
    -}
        
