module Huffman {-(statistics, maketree, encode, decode, Htree)-} where
import Data.Ord (comparing)
import Data.List (find, delete)
import Data.Maybe (isJust, fromJust)
import Distribution.Simple.Program.HcPkg (list)
import Data.Char(intToDigit)

import GHC.Char

data Htree = Leaf Char | Branch Htree Htree

data Wtree = L Integer Char | B Integer Wtree Wtree

-- COMPLETED --
-- Statistics -----------------------------------------------------------------------------
statistics :: String -> [(Integer, Char)]
statistics text = filtering (statRec text (replicate 256 0))


statRec :: String -> [Integer] -> [(Integer, Char)]
statRec [] list = zip list (map chr [0..255])
statRec (c:rest) list = statRec rest (replaceAtIndex (fromEnum c) ((list!!fromEnum c)+1) list)


filtering :: [(Integer, Char)] -> [(Integer, Char)]
filtering = filter (\(x,_) -> x /= 0)


replaceAtIndex :: Int -> Integer -> [Integer] -> [Integer]
replaceAtIndex index numb list =    let (x,_:y) = splitAt index list
                                    in x++numb:y


letterExistInList :: Char -> [(Integer, Char)] -> Bool
letterExistInList cha list = isJust (find (\(_,x) -> x == cha) list)


findTuple :: Char -> [(Integer, Char)] -> Maybe (Integer, Char)
findTuple c = find (\(_,x) -> x == c)


-- //TODO MakeTree
-- MakeTree --------------------------------------------------------------------------------------------
maketree :: [(Integer, Char)] -> Htree
maketree [_] = error "Not Implemented Yet"


-- //TODO Encode
-- Encode ------------------------------------------------------------------------------------------
encode :: String -> (Htree , [Integer])
encode _ = error "Not Implemented Yet"


-- //TODO Decode
-- Decode -------------------------------------------------------------------------------------
decode :: Htree -> [Integer] -> String
decode _ _ = error "Not Implemented Yet"