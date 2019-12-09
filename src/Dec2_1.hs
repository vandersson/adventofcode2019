module Dec2_1 where

import           Data.Either
import qualified Data.Text      as Text
import qualified Data.Text.IO   as Text
import qualified Data.Text.Read as Text
import Data.List

readInput :: Integral a => String -> IO [a]
readInput file = do
  rawInput <- fmap (Text.split (==',')) $ Text.readFile file
  return
    $ map (\(a, _) -> a)
    $ rights
    $ map (Text.decimal) rawInput


replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt val index l =
  (\(start,_:end) -> start ++ val:end)
  $ Data.List.splitAt index l

replaceOutput :: Int -> [Int] -> Int -> [Int]
replaceOutput index l val =
  replaceAt val (l !! (index+3)) l

nextIndex :: Int -> Int
nextIndex = (+) 4 

valAt :: [Int] -> Int -> Int
valAt l =
  (!!) l . (!!) l
  

op :: (Int -> Int -> Int) -> Int -> [Int] -> Int
op f i l =
  f (valAt l (i + 1)) (valAt l (i + 2))

add :: Int -> [Int] -> Int
add opIndex l =
  op (+) opIndex l

mult :: Int -> [Int] -> Int
mult opIndex l =
  op (*) opIndex l

eval :: Int -> [Int] -> [Int]
eval currentIndex mem
  | current == 99 = mem
  | current == 1 = eval (nextIndex currentIndex) $ replaceOutput currentIndex mem $ add currentIndex mem
  | current == 2 = eval (nextIndex currentIndex) $ replaceOutput currentIndex mem $ mult currentIndex mem
  | otherwise = error ("Error in control char: " ++ show current)
  where current = mem !! currentIndex




main :: IO ()
main = do
  input <- readInput "src/input2.txt"
  print $ Data.List.head $ eval 0 $ replaceAt 2 2 $ replaceAt 12 1 $ input
