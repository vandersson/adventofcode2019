module Dec1_2 where

import qualified Dec1 as One

moduleFuel :: Int -> Int
moduleFuel fuelWeight =
  let fuel = One.fuelForModule fuelWeight
  in
    if fuel > 0 then fuel + moduleFuel fuel
    else 0


totalFuel :: [Int] -> Int
totalFuel = foldl (+) 0 . map moduleFuel

main :: IO ()
main = do
  input <- One.readInput  
  print
    $ totalFuel
    $ input
  
