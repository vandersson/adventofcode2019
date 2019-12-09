module Dec1 where

import           Data.Either
import qualified Data.Text      as Text
import qualified Data.Text.IO   as Text
import qualified Data.Text.Read as Text

fuelForModule :: Int -> Int
fuelForModule mass = (\i -> i - 2) $ div mass  3

totalFuel :: [Int] -> Int
totalFuel = foldl (+) 0 . map fuelForModule

readInput :: Integral a => IO [a]
readInput = do
  rawInput <- fmap Text.lines $ Text.readFile "input1.txt"
  return
    $ map (\(a, _) -> a)
    $ rights
    $ map (Text.decimal) rawInput

main :: IO ()
main = do
  input <- readInput
  print
    $ totalFuel
    $ input
    
