module Test.Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Test.QuickCheck
import Data.Tuple( fst, snd )

import Main

crumbify1 :: String -> Int -> Boolean
crumbify1 desc minutes = 
  minutes == length returned
  where returned = fst $ crumbify desc minutes

crumbify2 :: String -> Int -> Boolean
crumbify2 desc minutes = (length rest) <= ((length desc) - minutes)
  where rest = snd $ crumbify desc minutes

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
     quickCheck crumbify1
     quickCheck crumbify2

