module Huffman where

data Htree = Leaf Char | Branch Htree Htree

data Wtree = L Integer Char | B Integer Wtree Wtree


statistics :: String -> [(Integer, Char)]



maketree :: [(Integer, Char)] -> Htree



encode :: String -> (Htree , [Integer])



decode :: Htree -> [Integer] -> String