module Utils (
  removeDups,
  newNameForVar
) where

import Data.Char

addIfNotIn :: (Eq a) => a -> [a] -> [a]
addIfNotIn x xs 
  | x `elem` xs = xs
  | otherwise = x:xs

removeDups :: (Eq a) => [a] -> [a]
removeDups = foldr addIfNotIn []

stringWillOverflowZ :: Char -> Bool
stringWillOverflowZ c = ((+1) . ord $ c) >= 123

nextValue :: String -> String
nextValue [] = "a"
nextValue xs
  | stringWillOverflowZ $ last xs = nextValue (init xs) ++ "a"
  | otherwise = let next = chr ((+1) . ord $ last xs) in (init xs) ++ [next]

newNameForVar :: String -> [String] -> String
newNameForVar _ fv = nextValue $ foldr max "" fv 
