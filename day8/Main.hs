module Main where

import Data.List
import Data.List.Split

layerify :: [a] -> [[a]]
layerify = chunksOf (25 * 6)

minLayer :: [[Char]] -> [Char]
minLayer = minimumBy (\a b -> compare (zeros a) (zeros b))
  where zeros = length . filter (== '0')

fingerPrint :: [Char] -> Int
fingerPrint layer =
  let ones = length (filter (== '1') layer)
      twos = length (filter (== '2') layer)
  in ones * twos

--

data Pixel
  = Trans
  | White
  | Black
  deriving (Eq, Ord)

instance Semigroup Pixel where
  Trans <> c = c
  Black <> c = Black
  White <> c = White

type Layer = [Pixel]

toPixel :: Char -> Pixel
toPixel '0' = Black
toPixel '1' = White
toPixel '2' = Trans

fromPixel :: Pixel -> Char
fromPixel Black = ' '
fromPixel White = '#'
fromPixel Trans = ' '

flatten :: [Layer] -> Layer
flatten = foldl1 (zipWith (<>))

main :: IO ()
main = do
  spaceImage <- init <$> readFile "input"
  print (fingerPrint . minLayer . layerify $ spaceImage)
  putStrLn (unlines . chunksOf 25 . map fromPixel . flatten . layerify . map toPixel $ spaceImage)
